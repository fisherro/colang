#lang colang

;; Comprehensive test suite for Colang predicates

(displayln "=======================================================")
(displayln "                 Predicate Tests")
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

(displayln "Testing routine? and resumable? predicates...")
(displayln "")

;; Test 1: routine? with various routine forms
(routine shorthand-routine (x) (* x 2))
(define explicit-routine (routine (x) (+ x 1)))
(define anon-routine (routine (x) (- x 1)))

(test-check "routine? recognizes shorthand routine" (routine? shorthand-routine))
(test-check "routine? recognizes explicit routine" (routine? explicit-routine))
(test-check "routine? recognizes anonymous routine" (routine? anon-routine))

;; Test 2: routine? with non-routines
(define regular-var 42)
(define string-var "hello")

(test-check "routine? rejects number" (not (routine? regular-var)))
(test-check "routine? rejects string" (not (routine? string-var)))
(test-check "routine? rejects boolean" (not (routine? #t)))
(test-check "routine? rejects list" (not (routine? '(1 2 3))))

;; Test 3: routine? with instances
(define test-inst (new shorthand-routine 5))
(test-check "routine? recognizes routine itself" (routine? shorthand-routine))
(test-check "routine? rejects routine instance" (not (routine? test-inst)))

;; Test 4: resumable? predicate lifecycle
(routine lifecycle-routine (x)
  (yield (* x 2))
  (* x 3))

(define lifecycle-inst (new lifecycle-routine 4))
(test-check "Fresh instance is resumable" (resumable? lifecycle-inst))

; First activation - should yield
(define first-result (lifecycle-inst))
(test-check "After first yield, still resumable" (resumable? lifecycle-inst))
(test-check "First activation yielded correctly" (= first-result 8))

; Second activation - should return final result
(define final-result (lifecycle-inst))
(test-check "After completion, not resumable" (not (resumable? lifecycle-inst)))
(test-check "Final activation returned correctly" (= final-result 12))

;; Test 5: resumable? with non-yielding routine
(routine simple-routine (x) (* x x))
(define simple-inst (new simple-routine 5))
(test-check "Non-yielding instance starts resumable" (resumable? simple-inst))

(define simple-result (simple-inst))
(test-check "Non-yielding instance finishes correctly" (= simple-result 25))
(test-check "Non-yielding instance not resumable after completion" (not (resumable? simple-inst)))

;; Test 6: resumable? with non-instances
(test-check "resumable? rejects routine itself" (not (resumable? simple-routine)))
(test-check "resumable? rejects number" (not (resumable? 42)))
(test-check "resumable? rejects string" (not (resumable? "hello")))

;; Test 7: resumable? with multiple yields
(routine multi-yield (n)
  (define i 0)
  (while (< i n)
    (yield i)
    (set! i (+ i 1)))
  "finished")

(define multi-inst (new multi-yield 3))
(test-check "Multi-yield starts resumable" (resumable? multi-inst))

(multi-inst) ; yield 0
(test-check "After first yield, still resumable" (resumable? multi-inst))

(multi-inst) ; yield 1  
(test-check "After second yield, still resumable" (resumable? multi-inst))

(multi-inst) ; yield 2
(test-check "After third yield, still resumable" (resumable? multi-inst))

(multi-inst) ; return "finished"
(test-check "After all yields, not resumable" (not (resumable? multi-inst)))

;; Test 8: Predicates with zero-parameter routine
(routine no-params ()
  (yield "middle")
  "end")

(test-check "Zero-param routine recognized" (routine? no-params))

(define no-param-inst (new no-params))
(test-check "Zero-param instance starts resumable" (resumable? no-param-inst))
(test-check "Zero-param instance yields correctly" (equal? (no-param-inst) "middle"))
(test-check "Zero-param instance returns correctly" (equal? (no-param-inst) "end"))
(test-check "Zero-param instance finishes" (not (resumable? no-param-inst)))

(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All predicate tests passed!")
    (displayln "💥 Some predicate tests failed!"))
(displayln "=======================================================")
(displayln "")