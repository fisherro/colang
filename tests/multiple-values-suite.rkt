#lang colang

;; Comprehensive test suite for multiple-value yield patterns

(displayln "=======================================================")
(displayln "             Multiple-Value Yield Tests")
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

(displayln "Testing multiple-value yield patterns...")
(displayln "")

;; Test 1: Basic multiple value yield
(routine multi-yield-basic ()
  (yield 1 2 3)
  "done")

(displayln "--- Test 1: Basic multiple value yield ---")
(define multi1 (new multi-yield-basic))
(let-values (((a b c) (multi1)))
  (test-check "Multi-yield first value" (= a 1))
  (test-check "Multi-yield second value" (= b 2))
  (test-check "Multi-yield third value" (= c 3)))
(test-check "Multi-yield final result" (equal? (multi1) "done"))

;; Test 2: Multiple yields with different value counts
(routine variable-yields (n)
  (if (= n 1)
      (yield 42)
      (if (= n 2)
          (yield 10 20)
          (yield 1 2 3 4 5)))
  "finished")

(displayln "--- Test 2: Variable yield counts ---")
(define var1 (new variable-yields 1))
(test-check "Single value yield" (= (var1) 42))

(define var2 (new variable-yields 2))
(let-values (((x y) (var2)))
  (test-check "Two value yield - first" (= x 10))
  (test-check "Two value yield - second" (= y 20)))

(define var3 (new variable-yields 3))
(let-values (((a b c d e) (var3)))
  (test-check "Five value yield - first" (= a 1))
  (test-check "Five value yield - third" (= c 3))
  (test-check "Five value yield - fifth" (= e 5)))

;; Test 3: Zero-argument yield
(routine zero-yield ()
  (yield)
  "completed")

(displayln "--- Test 3: Zero-argument yield ---")
(define zero1 (new zero-yield))
;; For zero-value yield, we'll just call it and check it doesn't error
(zero1)  ; This should succeed without values
(test-check "Zero-argument yield executes without error" #t)
(test-check "Zero-yield final result" (equal? (zero1) "completed"))

;; Test 4: Multiple-value resume arguments
(routine multi-resume ()
  (let-values (((a b c) (yield "ready")))
    (displayln (string-append "Resumed with: " 
                             (number->string a) " " 
                             (number->string b) " " 
                             (number->string c)))
    (+ a b c)))

(displayln "--- Test 4: Multiple-value resume arguments ---")
(define resume1 (new multi-resume))
(test-check "Multi-resume initial yield" (equal? (resume1) "ready"))
(test-check "Multi-resume with three values" (= (resume1 10 20 30) 60))

;; Test 5: Nested let-values with yields
(routine nested-values ()
  (let-values (((x y) (yield 5 10)))
    (let-values (((a b c) (yield x y (+ x y))))
      (+ x y a b c))))

(displayln "--- Test 5: Nested let-values ---")
(define nested1 (new nested-values))
(let-values (((first second) (nested1)))
  (test-check "Nested first yield - first value" (= first 5))
  (test-check "Nested first yield - second value" (= second 10)))

;; Resume with values for the first yield
(let-values (((a b c) (nested1 7 14)))
  (test-check "Nested second yield - first" (= a 7))
  (test-check "Nested second yield - second" (= b 14))
  (test-check "Nested second yield - third" (= c 21)))

;; Resume with values for the second yield  
(test-check "Nested final calculation" (= (nested1 1 2 3) 27)) ; 7+14+1+2+3

;; Test 6: Multiple values with different types
(routine mixed-types ()
  (yield 42 "hello" #t '(1 2 3))
  "mixed-done")

(displayln "--- Test 6: Multiple values with different types ---")
(define mixed1 (new mixed-types))
(let-values (((num str bool lst) (mixed1)))
  (test-check "Mixed types - number" (= num 42))
  (test-check "Mixed types - string" (equal? str "hello"))
  (test-check "Mixed types - boolean" (equal? bool #t))
  (test-check "Mixed types - list" (equal? lst '(1 2 3))))

;; Test 7: Yield in conditional with multiple values
(routine conditional-multi (flag)
  (if flag
      (yield 1 2)
      (yield 3 4))
  "conditional-done")

(displayln "--- Test 7: Conditional multiple yields ---")
(define cond1 (new conditional-multi #t))
(let-values (((a b) (cond1)))
  (test-check "Conditional true - first" (= a 1))
  (test-check "Conditional true - second" (= b 2)))

(define cond2 (new conditional-multi #f))
(let-values (((c d) (cond2)))
  (test-check "Conditional false - first" (= c 3))
  (test-check "Conditional false - second" (= d 4)))

;; Test 8: Quick activation with multiple values
(routine quick-multi ()
  (yield 100 200 300)
  "quick-done")

(displayln "--- Test 8: Quick activation with multiple values ---")
(let-values (((x y z) (quick-multi)))
  (test-check "Quick activation multi - first" (= x 100))
  (test-check "Quick activation multi - second" (= y 200))
  (test-check "Quick activation multi - third" (= z 300)))

;; Test 9: Multiple yields in sequence
(routine sequence-multi ()
  (yield 1 2)
  (yield 3 4 5)
  (yield 6)
  "sequence-done")

(displayln "--- Test 9: Sequential multiple yields ---")
(define seq1 (new sequence-multi))
(let-values (((a b) (seq1)))
  (test-check "Sequence first yield - values" (and (= a 1) (= b 2))))

(let-values (((c d e) (seq1)))
  (test-check "Sequence second yield - values" (and (= c 3) (= d 4) (= e 5))))

(test-check "Sequence third yield - single value" (= (seq1) 6))
(test-check "Sequence final result" (equal? (seq1) "sequence-done"))

;; Test 10: Multiple value consistency
(routine consistency-test ()
  (yield 1 2 3)
  "consistency-done")

(displayln "--- Test 10: Multiple value consistency ---")
(define consistency1 (new consistency-test))

;; Test that we get the expected values consistently
(let-values (((a b c) (consistency1)))
  (test-check "Consistency - all three values" (and (= a 1) (= b 2) (= c 3))))

(test-check "Consistency final result" (equal? (consistency1) "consistency-done"))

(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All multiple-value yield tests passed!")
    (displayln "💥 Some multiple-value yield tests failed!"))
(displayln "=======================================================")
(displayln "")