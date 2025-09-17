#lang racket
(require racket/control)

(provide (rename-out [colang-module-begin #%module-begin])
         (except-out (all-from-out racket) define #%app #%module-begin)
         (rename-out [colang-define define]
                     [colang-app #%app])
         yield resumable? while set! new)

; Coroutine instance structure
(struct coroutine-instance (raw-func args cont active?) 
  #:mutable
  #:property prop:procedure
  (lambda (self . resume-args)
    (apply activate-coroutine self resume-args))
  #:transparent)

; Store the raw coroutine functions separately
(define coroutine-registry (make-hash))

; Current coroutine parameter
(define current-coroutine (make-parameter #f))

; Transform all function definitions to create coroutine functions
(define-syntax colang-define
  (syntax-rules ()
    [(_ (name params ...) body ...)
     (begin
       ; Store the raw function body
       (hash-set! coroutine-registry 'name (lambda (params ...) body ...))
       ; Define the public function that creates temporary instances and activates them
       (define (name . args)
         (let ([temp-instance (coroutine-instance 
                               'name  
                               args   ; args become the stored parameters
                               #f     
                               #f)])  
           (activate-coroutine temp-instance))))]  ; no args for activation
    [(_ name value)
     (define name value)]))

; Create new coroutine instance with parameters
(define-syntax new
  (syntax-rules ()
    [(_ coroutine-func args ...)
     (coroutine-instance 'coroutine-func (list args ...) #f #f)]))

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
     (let* ([raw-func (hash-ref coroutine-registry (coroutine-instance-raw-func instance))]
            [stored-args (coroutine-instance-args instance)]
            [all-args (append stored-args args)])
       (parameterize ([current-coroutine instance])
         (prompt 
           (apply raw-func all-args))))]
    
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