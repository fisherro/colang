#lang racket
(require racket/control)

(provide (rename-out [colang-module-begin #%module-begin])
         (except-out (all-from-out racket) define #%app #%module-begin)
         (rename-out [colang-define define]
                     [colang-app #%app])
         yield resumable? while set! new routine)

; Coroutine instance structure
(struct coroutine-instance (func args cont active?) 
  #:mutable
  #:property prop:procedure
  (lambda (self . resume-args)
    (apply activate-coroutine self resume-args))
  #:transparent)

; Current coroutine parameter
(define current-coroutine (make-parameter #f))

; Transform define to handle only variable definitions
(define-syntax colang-define
  (syntax-rules ()
    [(_ name value)
     (define name value)]))

; Routine wrapper structure to support both quick activation and new
(struct routine-wrapper (func)
  #:property prop:procedure
  (lambda (self . args)
    ; Quick activation: create temp instance and activate
    (let ([temp-instance (coroutine-instance (routine-wrapper-func self) args #f #f)])
      (activate-coroutine temp-instance))))

; Create new coroutine instance with parameters
(define (new routine-wrapper . args)
  (if (routine-wrapper? routine-wrapper)
      (coroutine-instance (routine-wrapper-func routine-wrapper) args #f #f)
      (error "Not a routine" routine-wrapper)))

; Create anonymous routine and shorthand syntax  
(define-syntax routine
  (lambda (stx)
    (syntax-case stx ()
      ; Shorthand definition form: (routine NAME (PARAM...) BODY...)
      [(_ name (params ...) body ...)
       (identifier? #'name)
       #'(define name (routine-wrapper (lambda (params ...) body ...)))]
      ; Anonymous form: (routine (PARAM...) BODY...)
      [(_ (params ...) body ...)
       #'(routine-wrapper (lambda (params ...) body ...))])))

; Yield zero or more values and capture continuation
(define (yield . vals)
  (control k
    (let ([cor (current-coroutine)])
      (when cor
        (set-coroutine-instance-cont! cor k))
      (apply values vals))))

; Activate a coroutine instance
(define (activate-coroutine instance . args)
  (cond
    ; First activation - combine stored parameters with activation args
    [(not (coroutine-instance-active? instance))
     (set-coroutine-instance-active?! instance #t)
     (let* ([func (coroutine-instance-func instance)]
            [stored-args (coroutine-instance-args instance)]
            [all-args (append stored-args args)])
       (parameterize ([current-coroutine instance])
         (prompt 
           (apply func all-args))))]
    
    ; Resume from continuation - use activation args for resume values
    [(coroutine-instance-cont instance)
     (let ([cont (coroutine-instance-cont instance)])
       (set-coroutine-instance-cont! instance #f)
       (parameterize ([current-coroutine instance])
         (prompt
           (apply cont args))))]
    
    ; Coroutine finished
    [else #f]))

; Check if coroutine can be called (either fresh or suspended)
(define (resumable? instance)
  (and (coroutine-instance? instance)
       (or (not (coroutine-instance-active? instance))  ; Fresh instance
           (coroutine-instance-cont instance))))

; Simple while loop
(define-syntax while
  (syntax-rules ()
    [(_ condition body ...)
     (let loop ()
       (when condition
         body ...
         (loop)))]))

; Transform all function applications
(define-syntax colang-app
  (syntax-rules ()
    [(_ f args ...)
     (f args ...)]))

(define-syntax colang-module-begin
  (syntax-rules ()
    [(_ body ...)
     (#%module-begin body ...)]))