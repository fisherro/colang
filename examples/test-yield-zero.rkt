#lang colang

(define (test-yield-zero)
  (displayln "Before yield")
  (yield)
  (displayln "After yield")
  "done")

(define (test-yield-multiple)
  (displayln "Before yield")
  (let-values (((a b c) (yield 1 2 3)))
    (displayln a)
    (displayln b)
    (displayln c))
  "done")

(displayln "=== Testing yield with zero arguments ===")
(define cor-zero (new test-yield-zero))
(cor-zero)
(displayln (cor-zero))

(displayln "=== Testing yield with multiple arguments ===")
(define cor-multi (new test-yield-multiple))
(cor-multi)
(displayln (cor-multi 10 20 30))