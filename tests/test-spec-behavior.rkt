#lang colang

(routine test-routine (x)
  (displayln "Before yield")
  (yield x)
  (displayln "After yield - this should not appear in quick activation")
  "final-result")

(displayln "=== Quick activation ===")
(define result1 (test-routine 42))
(displayln "Returned:")
(displayln result1)

(displayln "=== Instance activation ===")
(define inst (new test-routine 42))
(define result2 (inst))
(displayln "First activation returned:")
(displayln result2)
(define result3 (inst))
(displayln "Second activation returned:")
(displayln result3)