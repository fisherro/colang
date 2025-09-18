#lang racket
(require racket/control)

(provide (rename-out [colang-module-begin #%module-begin])
         (except-out (all-from-out racket) define #%app #%module-begin)
         (rename-out [colang-define define]
                     [colang-app #%app])
         yield resumable? while set! new routine)

; Coroutine instance structure
(struct coroutine-instance (raw-func args cont active?) 
  #:mutable
  #:property prop:procedure
  (lambda (self . resume-args)
    (apply activate-coroutine self resume-args))
  #:transparent)

; Store the raw coroutine functions separately
(define coroutine-registry (make-hash))

; Store routine metadata for anonymous routines
(define routine-metadata (make-weak-hash))

; Current coroutine parameter
(define current-coroutine (make-parameter #f))

; Transform define to handle only variable definitions
(define-syntax colang-define
  (syntax-rules ()
    [(_ name value)
     (define name value)]))

; Create new coroutine instance with parameters
(define (new coroutine-func . args)
  (cond
    ; If it's a procedure (anonymous routine), look up its routine-id
    [(procedure? coroutine-func)
     (let ([routine-id (hash-ref routine-metadata coroutine-func #f)])
       (if routine-id
           (coroutine-instance routine-id args #f #f)
           (error "Not a routine procedure")))]
    ; If it's a symbol (named routine), use it directly
    [else
     (coroutine-instance coroutine-func args #f #f)]))

; Create anonymous routine and shorthand syntax
(define-syntax routine
  (syntax-rules ()
    ; Shorthand form: (routine NAME (PARAM...) BODY...)
    [(_ name (params ...) body ...)
     (define name 
       (let ([routine-id (gensym 'routine)])
         ; Store the raw function body with generated symbol
         (hash-set! coroutine-registry routine-id (lambda (params ...) body ...))
         ; Create the routine procedure
         (let ([routine-proc (lambda args
                               (let ([temp-instance (coroutine-instance 
                                                     routine-id
                                                     args   ; args become the stored parameters
                                                     #f     
                                                     #f)])  
                                 (activate-coroutine temp-instance)))])
           ; Store metadata to link procedure to routine-id
           (hash-set! routine-metadata routine-proc routine-id)
           routine-proc)))]
    ; Anonymous form: (routine (PARAM...) BODY...)
    [(_ (params ...) body ...)
     (let ([routine-id (gensym 'routine)])
       ; Store the raw function body with generated symbol
       (hash-set! coroutine-registry routine-id (lambda (params ...) body ...))
       ; Create the routine procedure
       (let ([routine-proc (lambda args
                             (let ([temp-instance (coroutine-instance 
                                                   routine-id
                                                   args   ; args become the stored parameters
                                                   #f     
                                                   #f)])  
                               (activate-coroutine temp-instance)))])
         ; Store metadata to link procedure to routine-id
         (hash-set! routine-metadata routine-proc routine-id)
         routine-proc))]))

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