#lang colang

;; Comprehensive test suite for Colang construction and activation

(displayln "=======================================================")
(displayln "           Construction and Activation Tests")
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

(displayln "Testing construction and activation patterns...")
(displayln "")

;; Test 1: Basic construction with new
(routine square (x) (* x x))
(define square-inst (new square 5))
(test-check "New instance is resumable" (resumable? square-inst))
(test-check "New instance computes correctly" (= (square-inst) 25))
(test-check "After computation, not resumable" (not (resumable? square-inst)))

;; Test 2: Construction with multiple arguments
(routine add-three (x y z) (+ x y z))
(define add-inst (new add-three 1 2 3))
(test-check "Multi-arg construction works" (= (add-inst) 6))

;; Test 3: Construction with partial arguments
(routine multiply-three (x y z) (* x y z))
(define partial-inst (new multiply-three 2))
(test-check "Partial construction instance is resumable" (resumable? partial-inst))
(test-check "Providing remaining args works" (= (partial-inst 3 4) 24))

;; Test 4: Quick activation equivalence
(routine double (x) (* x 2))
(test-check "Quick activation works" (= (double 7) 14))
(test-check "Quick activation equals instance activation" (= (double 7) ((new double) 7)))

;; Test 5: Quick activation with yielding routine
(routine yielding-double (x)
  (yield (* x 2))
  (* x 4))

(test-check "Quick activation returns first yield" (= (yielding-double 3) 6))

;; Compare with full instance
(define full-inst (new yielding-double 3))
(test-check "Instance first activation matches quick" (= (full-inst) 6))
(test-check "Instance second activation gives final result" (= (full-inst) 12))

;; Test 6: Arguments during activation
(routine message-handler ()
  (define msg (yield "waiting"))
  (string-append "Handled: " msg))

(define handler-inst (new message-handler))
(test-check "Handler initial state" (equal? (handler-inst) "waiting"))
(test-check "Handler with argument" (equal? (handler-inst "test") "Handled: test"))

;; Test 7: Mixed construction and activation arguments
(routine formatter (prefix)
  (define suffix (yield "ready"))
  (string-append prefix "-" suffix))

(define format-inst (new formatter "LOG"))
(test-check "Formatter with construction arg" (equal? (format-inst) "ready"))
(test-check "Formatter with activation arg" (equal? (format-inst "ERROR") "LOG-ERROR"))

;; Test 8: Zero-parameter routine construction
(routine get-pi () 3.14159)
(define pi-inst (new get-pi))
(test-check "Zero-param construction works" (= (pi-inst) 3.14159))

;; Test 9: Anonymous routine construction
(define anon-cube (routine (x) (* x x x)))
(define cube-inst (new anon-cube 3))
(test-check "Anonymous routine construction works" (= (cube-inst) 27))

;; Test 10: Complex argument types
(routine list-processor (lst)
  (for ([item lst])
    (yield item))
  "processed")

(define list-inst (new list-processor '(a b c)))
(test-check "List processing - first item" (equal? (list-inst) 'a))
(test-check "List processing - second item" (equal? (list-inst) 'b))
(test-check "List processing - third item" (equal? (list-inst) 'c))
(test-check "List processing - completion" (equal? (list-inst) "processed"))

(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All construction and activation tests passed!")
    (displayln "💥 Some construction and activation tests failed!"))
(displayln "=======================================================")
(displayln "")