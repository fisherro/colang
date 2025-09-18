#lang colang

; Test only shorthand syntax at top level
(routine test-shorthand (x) (* x 2))

(displayln (test-shorthand 10))

(displayln "Shorthand test complete")