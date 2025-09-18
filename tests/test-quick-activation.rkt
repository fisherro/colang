#lang colang

(routine simple (x)
  (displayln "Simple routine called with:")
  (displayln x)
  (* x x))

(displayln "=== Testing quick activation ===")
(displayln "Calling (simple 5) directly:")
(displayln (simple 5))

(displayln "=== Testing with new ===")
(define inst (new simple 6))
(displayln "Calling instance:")
(displayln (inst))