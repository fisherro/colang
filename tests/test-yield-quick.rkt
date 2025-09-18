#lang colang

(routine yielding (x)
  (displayln "Before yield")
  (yield x)
  (displayln "After yield")
  "done")

(displayln "=== Testing quick activation with yield ===")
(displayln "Result:")
(displayln (yielding 42))

(displayln "=== Testing with new and multiple calls ===")
(define inst (new yielding 99))
(displayln "First call:")
(displayln (inst))
(displayln "Second call:")
(displayln (inst))