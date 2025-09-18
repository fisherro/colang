#lang colang

; ================
; ROUTINES TO TEST
; ================

(routine fibonacci-generator ()
  (define a 0)
  (define b 1)
  (while #t
    (yield a)
    (define next (+ a b))
    (set! a b)
    (set! b next)))

(routine take (n source)
  (define i 0)
  (while (and (< i n)
              (resumable? source))
    (yield (source))
    (set! i (+ i 1))))

(routine for-each (function source)
  (while (resumable? source)
    (let ((datum (source)))
      (unless (void? datum)
        (function datum)))))

(routine filter (predicate generator)
  (while (resumable? generator)
    (let ((result (generator)))
      (when (predicate result)
        (yield result)))))

(routine transform (function source)
  (while (resumable? source)
    (yield (function (source)))))

; Additional ideas
; partial-left/partial-right: partial specialization
; compose: function composition
; enumerate: add a count to each item of a source

; ==============
; GENERATOR TEST
; ==============

(routine generator-test ()
  (displayln "=== GENERATOR TEST ===")
  (define fib (new fibonacci-generator))
  (define i 0)
  (while (< i 10)
  (display "F")
  (display i)
  (display " = ")
  (displayln (fib))
  (set! i (+ i 1)))
  (displayln ""))
(generator-test)

; =========
; TAKE TEST
; =========

(routine take-test ()
  (displayln "=== TAKE TEST ===")
  (define fib (new fibonacci-generator))
  (define first10 (new take 10 fib))
  (while (resumable? first10)
    (display "Taken: ")
    (displayln (first10)))
  (displayln ""))
(take-test)

; =============
; FOR-EACH TEST
; =============

(routine for-each-test ()
  (displayln "=== FOR-EACH TEST ===")
  (define fib (new fibonacci-generator))
  (define first10 (new take 10 fib))
  (for-each displayln first10)
  (displayln ""))
(for-each-test)

; ===========
; FILTER TEST
; ===========

(routine filter-test ()
  (displayln "=== FILTER TEST ===")
  (define fib (new fibonacci-generator))
  (define evens (new filter even? fib))
  (define i 0)
  (while (< i 10)
  (display "Even Fibonacci: ")
  (displayln (evens))
  (set! i (+ i 1)))
  (displayln ""))
(filter-test)

; ==============
; TRANSFORM TEST
; ==============

(routine transform-test ()
  (displayln "=== SQUARE FIBONACCI TEST ===")
  (routine square (n)
    (* n n))
  (define fib (new fibonacci-generator))
  (define transformer (new transform square fib))
  (define i 0)
  (while (< i 10)
  (display "Square Fibonacci: ")
  (displayln (transformer))
  (set! i (+ i 1)))
  (displayln ""))
(transform-test)

; =====================
; FILTER-TRANSFORM TEST
; =====================

(routine filter-transform-test ()
  (displayln "=== FILTER-TRANSFORM TEST ===")
  (routine square (n)
    (* n n))
  (define fib (new fibonacci-generator))
  (define evens (new filter even? fib))
  (define transformer (new transform square evens))
  (define i 0)
  (while (< i 10)
  (display "Square of Even Fibonacci: ")
  (displayln (transformer))
  (set! i (+ i 1)))
  (displayln ""))
(filter-transform-test)

; =============
; PRIME NUMBERS
; =============

; Simple primality test
(routine prime? (n)
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

(routine prime-test ()
  (displayln "=== PRIME NUMBERS ===")
  (define fib (new fibonacci-generator))
  (define evens (new filter prime? fib))
  (define i 0)
  (while (< i 10)
  (display "Prime: ")
  (displayln (evens))
  (set! i (+ i 1)))
  (displayln ""))
(prime-test)
