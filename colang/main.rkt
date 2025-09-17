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
       ; Define the public function that creates temporary instances
       (define (name . args)
         (let ([temp-instance (coroutine-instance 
                               'name  
                               args
                               #f     
                               #f)])  
           (apply activate-coroutine temp-instance args))))]
    [(_ name value)
     (define name value)]))

; Create new coroutine instance
(define-syntax new
  (syntax-rules ()
    [(_ coroutine-func)
     (coroutine-instance 'coroutine-func '() #f #f)]))

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
    ; First activation
    [(not (coroutine-instance-active? instance))
     (set-coroutine-instance-active?! instance #t)
     (set-coroutine-instance-args! instance args)
     (let ([raw-func (hash-ref coroutine-registry (coroutine-instance-raw-func instance))])
       (parameterize ([current-coroutine instance])
         (prompt 
           (apply raw-func args))))]
    
    ; Resume from continuation
    [(coroutine-instance-cont instance)
     (let ([cont (coroutine-instance-cont instance)])
       (set-coroutine-instance-cont! instance #f)
       (parameterize ([current-coroutine instance])
         (prompt
           (apply cont args))))]
    
    ; Coroutine finished
    [else #f]))

; Check if coroutine can be resumed
(define (resumable? instance)
  (and (coroutine-instance? instance)
       (coroutine-instance-active? instance)
       (coroutine-instance-cont instance)
       #t))

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