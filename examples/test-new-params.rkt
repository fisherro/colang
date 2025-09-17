#lang colang

; Simple test with the new parameter passing
(define (test-params x y)
  (displayln (string-append "Started with x=" (number->string x) " y=" (number->string y)))
  (yield (+ x y))
  (displayln "Resumed")
  (* x y))

(displayln "=== Testing new parameter passing ===")

; Test 1: Quick activation (should work as before)
(displayln "Quick activation:")
(displayln (test-params 3 4))

; Test 2: Instance with parameters during construction
(displayln "Instance construction with parameters:")
(define instance (new test-params 5 6))
(displayln "Created instance, now activating...")
(displayln (instance))  ; First activation
(displayln "Resuming...")
(displayln (instance))  ; Resume

; Test 3: Instance with no parameters (should fail gracefully)
(displayln "Testing parameter-less construction:")
(define (simple-test)
  (displayln "Simple test started")
  (yield 42)
  "done")

(define simple-instance (new simple-test))
(displayln (simple-instance))