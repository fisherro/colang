#lang colang

;; Comprehensive test suite for parameter passing patterns

(displayln "=======================================================")
(displayln "             Parameter Passing Tests")
(displayln "=======================================================")
(displayln "")

;; Track test results
(define passed 0)
(define failed 0)

(routine test-check (description condition)
  (if condition
      (begin
        (displayln (string-append "✅ " description))
        (set! passed (+ passed 1)))
      (begin
        (displayln (string-append "❌ " description))
        (set! failed (+ failed 1)))))

(displayln "Testing parameter passing patterns...")
(displayln "")

;; Test routine that shows parameter handling
(routine param-tracker (a b c)
  (displayln (string-append "Received: a=" (number->string a) 
                           " b=" (number->string b) 
                           " c=" (number->string c)))
  (yield (+ a b c))
  (define resume-value (yield (* a b c)))
  (+ a b c resume-value))

;; Test 1: All parameters to new
(displayln "--- Test 1: All parameters to new ---")
(define inst1 (new param-tracker 1 2 3))
(test-check "All params to new - sum" (= (inst1) 6))
(test-check "All params to new - product" (= (inst1) 6))
(test-check "All params to new - final with resume value" (= (inst1 10) 16))

;; Test 2: Split parameters (2 to new, 1 to activation)
(displayln "--- Test 2: Split parameters (2 to new, 1 to activation) ---")
(define inst2 (new param-tracker 4 5))
(test-check "Split params - first activation with remaining param" (= (inst2 6) 15))
(test-check "Split params - product" (= (inst2) 120))
(test-check "Split params - final with resume value" (= (inst2 20) 35))

;; Test 3: No parameters to new (all to activation)
(displayln "--- Test 3: No parameters to new ---")
(define inst3 (new param-tracker))
(test-check "No params to new - all params in first activation" (= (inst3 7 8 9) 24))
(test-check "No params to new - product" (= (inst3) 504))
(test-check "No params to new - final with resume value" (= (inst3 30) 54))

;; Test 4: Single parameter routine variations
(routine single-param (x)
  (yield (* x 2))
  (* x 3))

(displayln "--- Test 4: Single parameter variations ---")
(define single1 (new single-param 5))
(test-check "Single param to new" (= (single1) 10))
(test-check "Single param final result" (= (single1) 15))

(define single2 (new single-param))
(test-check "Single param to activation" (= (single2 7) 14))
(test-check "Single param activation final result" (= (single2) 21))

;; Test 5: Zero parameter routine
(routine zero-param ()
  (yield "first")
  "second")

(displayln "--- Test 5: Zero parameter routine ---")
(define zero1 (new zero-param))
(test-check "Zero param construction" (equal? (zero1) "first"))
(test-check "Zero param final result" (equal? (zero1) "second"))

;; Test 6: Complex parameter combinations with yield values
(routine param-yielder (a b)
  (define first-result (yield a))
  (define second-result (yield b))
  (+ a b first-result second-result))

(displayln "--- Test 6: Parameters with yield interactions ---")
(define yield-inst1 (new param-yielder 10 20))
(test-check "Yield interaction - first yield" (= (yield-inst1) 10))
(test-check "Yield interaction - second yield with resume" (= (yield-inst1 5) 20))
(test-check "Yield interaction - final with both resume values" (= (yield-inst1 15) 50))

;; Test 7: Quick activation parameter handling
(displayln "--- Test 7: Quick activation ---")
(test-check "Quick activation with all params" (= (single-param 4) 8))
(test-check "Quick activation complex routine" (= (param-tracker 1 1 1) 3))

;; Test 8: Partial application edge cases
(routine four-params (a b c d)
  (yield (+ a b))
  (yield (+ c d))
  (+ a b c d))

(displayln "--- Test 8: Multiple parameter splitting ---")
(define four1 (new four-params 1 2 3 4))
(test-check "Four params to new - first yield" (= (four1) 3))
(test-check "Four params to new - second yield" (= (four1) 7))
(test-check "Four params to new - final" (= (four1) 10))

(define four2 (new four-params 1 2))
(test-check "Two params to new, two to activation" (= (four2 3 4) 3))
(test-check "Two to new - second yield" (= (four2) 7))
(test-check "Two to new - final" (= (four2) 10))

(define four3 (new four-params))
(test-check "No params to new, all to activation" (= (four3 1 2 3 4) 3))
(test-check "All to activation - second yield" (= (four3) 7))
(test-check "All to activation - final" (= (four3) 10))

;; Test 9: Error handling for insufficient parameters
(routine needs-three (a b c) (+ a b c))

(displayln "--- Test 9: Parameter validation ---")
;; This should work fine since we're providing the missing parameter
(define partial-inst (new needs-three 1 2))
(test-check "Partial construction completed at activation" (= (partial-inst 3) 6))

;; Test 10: Complex parameter types
(routine complex-params (numbers strings)
  (yield (length numbers))
  (yield (length strings))
  (+ (apply + numbers) (apply + (map string-length strings))))

(displayln "--- Test 10: Complex parameter types ---")
(define complex-inst (new complex-params '(1 2 3 4) '("hello" "world")))
(test-check "Complex params - number count" (= (complex-inst) 4))
(test-check "Complex params - string count" (= (complex-inst) 2))
(test-check "Complex params - final calculation" (= (complex-inst) 20))

(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All parameter passing tests passed!")
    (displayln "💥 Some parameter passing tests failed!"))
(displayln "=======================================================")
(displayln "")