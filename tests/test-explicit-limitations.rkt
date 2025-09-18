#lang colang

(displayln "=== Testing explicit definition in various contexts ===")

; Test 1: Basic explicit definition (should work)
(define square-explicit (routine (x) (* x x)))
(displayln "Test 1 - basic explicit definition:")
(displayln (square-explicit 5))

; Test 2: Explicit definition with yield
(define counter-explicit (routine (n)
  (define i 0)
  (while (< i n)
    (yield i)
    (set! i (+ i 1)))
  "done"))

(displayln "Test 2 - explicit definition with yield, quick activation:")
(displayln (counter-explicit 3))

(displayln "Test 3 - explicit definition with yield, instance:")
(define c (new counter-explicit 3))
(displayln (c))
(displayln (c))
(displayln (c))
(displayln (c))

; Test 4: Explicit definition in local scope
(displayln "Test 4 - explicit definition in local scope:")
(let ()
  (define local-routine (routine (x) (+ x 100)))
  (displayln (local-routine 23)))

; Test 5: Explicit definition with complex expressions
(define complex-routine (routine (x y)
  (if (> x y)
      (yield x)
      (yield y))
  "finished"))

(displayln "Test 5 - complex explicit definition:")
(displayln (complex-routine 10 5))

; Test 6: Multiple explicit definitions
(define add-explicit (routine (x y) (+ x y)))
(define mul-explicit (routine (x y) (* x y)))
(displayln "Test 6 - multiple explicit definitions:")
(displayln (add-explicit 3 4))
(displayln (mul-explicit 3 4))