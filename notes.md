# Notes

Random and incomplete musings...

## Tests

We need an actual, automated test suite.

## Pipeline examples additions

### Pipeline builder

```racket
(define-syntax pipeline
  (syntax-rules (->)
    [(pipeline source -> stage1 -> stage2 -> ...)
     (let* ([s1 (stage1 source)]
            [s2 (stage2 s1)]
            ...)
       s2)]))

; Usage:
(pipeline (new fibonacci-generator)
          -> (curry filter even?)
          -> (curry transform square)
          -> (curry take 10))
```

### More combinators

```racket
(define (enumerate source)
  (define i 0)
  (while (resumable? source)
    (yield (list i (source)))
    (set! i (+ i 1))))

(define (zip source1 source2)
  (while (and (resumable? source1) (resumable? source2))
    (yield (list (source1) (source2)))))

(define (drop n source)
  (define i 0)
  (while (and (< i n) (resumable? source))
    (source)  ; discard
    (set! i (+ i 1)))
  (while (resumable? source)
    (yield (source))))
```

## Abort?

In the case where a coroutine doesn't know whether it is going to be able to supply a value until it is actually resumed...

Could we somehow use continuations behind-the-scenes to give the coroutine a way to abort back to the last call to `resumable?` and return false.

```racket
; Parameter to hold the resumable? abort continuation
(define current-resumable-abort (make-parameter #f))

; Enhanced resumable? that sets up abort target
(define (resumable? instance)
  (and (coroutine-instance? instance)
       (or (not (coroutine-instance-active? instance))  ; Fresh instance
           (and (coroutine-instance-cont instance)      ; Has continuation
                (call/cc 
                  (lambda (abort-k)
                    (parameterize ([current-resumable-abort abort-k])
                      #t)))))))  ; Return true, but mark abort point

; Function for coroutines to abort back to resumable?
(define (abort-resumable)
  (let ([abort-k (current-resumable-abort)])
    (if abort-k
        (abort-k #f)  ; Jump back to resumable? and make it return false
        (error "abort-resumable called outside of resumable? context"))))

; Modified activate-coroutine to handle abort context
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
```

Usage:

```racket
(define (uncertain-producer data-source)
  (while #t
    ; We get the actual resume arguments here
    (let ([filter-criteria (yield)])  ; Get criteria from previous activation
      ; Now we can check if we can produce a value based on the criteria
      (let ([available-data (find-data data-source filter-criteria)])
        (if available-data
            (yield available-data)
            (abort-resumable))))))  ; Abort back to the resumable? check

; Usage:
(define producer (new uncertain-producer my-data))
(if (resumable? producer)              ; Sets up abort target, returns true
    (let ([result (producer "urgent")])  ; Passes actual filter criteria
      (displayln result))              ; Only executes if producer didn't abort
    (displayln "No data available"))   ; Executes if producer called abort-resumable
```
