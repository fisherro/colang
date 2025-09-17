#lang colang

; =============================================================================
; COLANG PRODUCER-CONSUMER PIPELINE DEMONSTRATION
; =============================================================================
; A comprehensive showcase of coroutine composition patterns

; =============================================================================
; BASIC GENERATORS
; =============================================================================

; Infinite Fibonacci sequence
(define (fibonacci-generator)
  (define a 0)
  (define b 1)
  (while #t
    (yield a)
    (define next (+ a b))
    (set! a b)
    (set! b next)))

; =============================================================================
; EXAMPLE 1: Basic Generator Usage
; =============================================================================

(displayln "=== BASIC FIBONACCI GENERATOR ===")
(define fib (new fibonacci-generator))
(define i 0)
(while (< i 10)
  (displayln (string-append "F" (number->string i) " = " (number->string (fib))))
  (set! i (+ i 1)))
(displayln "")

; =============================================================================
; EXAMPLE 2: Manual Pipeline Processing
; =============================================================================

(displayln "=== MANUAL EVEN FIBONACCI FILTER ===")
(define fib2 (new fibonacci-generator))
(define even-found 0)
(while (< even-found 6)
  (define num (fib2))
  (when (even? num)
    (displayln (string-append "Even Fibonacci: " (number->string num)))
    (set! even-found (+ even-found 1))))
(displayln "")

; =============================================================================
; EXAMPLE 3: Coroutine Composition
; =============================================================================

; Composite coroutine: even fibonacci numbers only
(define (even-fibonacci-stream)
  (define source (new fibonacci-generator))
  (while #t
    (define value (source))
    (when (even? value)
      (yield value))))

(displayln "=== COMPOSED EVEN FIBONACCI STREAM ===")
(define even-stream (new even-fibonacci-stream))
(define collected 0)
(while (< collected 5)
  (displayln (string-append "Stream value: " (number->string (even-stream))))
  (set! collected (+ collected 1)))
(displayln "")

; =============================================================================
; EXAMPLE 4: Multi-Stage Processing Pipeline
; =============================================================================

(define (filter predicate source)
  ; First activation is initialization, so we don't yield a value.
  (yield)
  (while (resumable? source)
    (let ((result (source)))
      (when (predicate result)
        (yield result))))
  ; Because the last value from source might be odd, we may have to return a
  ; final placeholder value. We'll use false.
  #f)

(define (transform function source)
  ; First activation is initialization, so we don't yield a value.
  (yield)
  (while (resumable? source)
    (yield (function (source))))
  #f)

(displayln "my-transformed-fibonacci")
(define (my-transformed-fibonacci offset)
  (define fg (new fibonacci-generator))
  (define f (new filter))
  (define tf (new transform))
  (define (square-and-add value)
    (+ offset (* value value)))
  (define iteration 1)
  (f even? fg)
  (tf square-and-add f)
  (while (< iteration 5)
    (let ((result (tf)))
      (when result
        (set! iteration (+ iteration 1))
        (displayln result)))))
(my-transformed-fibonacci 1000)

; Complex transformation: Fibonacci -> Even -> Square -> Offset
(define (transformed-fibonacci offset)
  (define source (new fibonacci-generator))
  (while #t
    (define value (source))
    (when (even? value)
      (define squared (* value value))
      (yield (+ squared offset)))))

(displayln "=== MULTI-STAGE TRANSFORMATION PIPELINE ===")
(displayln "Fibonacci -> Even Filter -> Square -> Add 1000")
(define transformer (new transformed-fibonacci))
(transformer 1000)
(define results 0)
(while (< results 4)
  (displayln (string-append "Transformed: " (number->string (transformer))))
  (set! results (+ results 1)))
(displayln "")

; =============================================================================
; EXAMPLE 5: Practical Application - Prime Numbers
; =============================================================================

; Simple primality test
(define (prime? n)
  (cond
    [(< n 2) #f]
    [(= n 2) #t]
    [else
     (define factor 2)
     (define is-prime #t)
     (while (and (< factor n) is-prime)
       (when (= (modulo n factor) 0)
         (set! is-prime #f))
       (set! factor (+ factor 1)))
     is-prime]))

; Prime number generator using Fibonacci sequence as source
(define (fibonacci-primes)
  (define source (new fibonacci-generator))
  (while #t
    (define value (source))
    (when (prime? value)
      (yield value))))

(displayln "=== FIBONACCI PRIME NUMBERS ===")
(define fib-primes (new fibonacci-primes))
(define prime-count 0)
(while (< prime-count 6)
  (displayln (string-append "Fibonacci Prime: " (number->string (fib-primes))))
  (set! prime-count (+ prime-count 1)))
(displayln "")

; =============================================================================
; EXAMPLE 6: Data Processing Summary
; =============================================================================

; Collector coroutine that gathers first N values
(define (collector source n)
  (displayln (string-append "Collecting first " (number->string n) " values:"))
  (define count 0)
  (while (< count n)
    (define value (source))
    (displayln (string-append "  [" (number->string count) "] " (number->string value)))
    (set! count (+ count 1))))

(displayln "=== COLLECTION DEMONSTRATION ===")
(define final-stream (new even-fibonacci-stream))
(collector final-stream 4)

(displayln "")
(displayln "=== DEMONSTRATION COMPLETE ===")
(displayln "Colang's coroutines enable elegant pipeline composition:")
(displayln "• Infinite sequences (generators)")
(displayln "• Data filtering and transformation")
(displayln "• Composable processing stages")
(displayln "• Clean separation of concerns")
(displayln "• Readable, maintainable code")