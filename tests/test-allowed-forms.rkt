#lang colang

(displayln "=== Testing blocked forms ===")

; Test that value definitions still work
(define bar 'bar)
(if (symbol? bar)
  (displayln "Value definitions work!")
  (displayln "Whoops!"))

; Test that routine definitions work
(define baz (routine (x) x))
(if (routine? baz)
  (displayln "Routine definitions work!")
  (displayln "Whoops!"))

; Test that routine shorthand works
(routine test-routine (x) (* x 2))
(if (routine? test-routine)
  (displayln "Routine shorthand works!")
  (displayln "Whoops!"))

(displayln "Quick test:")
(displayln (test-routine 5))