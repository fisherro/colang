#lang colang

;; Comprehensive test suite for anonymous routine edge cases

(displayln "=======================================================")
(displayln "             Anonymous Routine Edge Cases")
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

(displayln "Testing anonymous routine edge cases...")
(displayln "")

;; Test 1: Anonymous routine assigned to variable
(displayln "--- Test 1: Anonymous routine assigned to variable ---")
(define square-anon (routine (x) (* x x)))
(test-check "Anonymous routine assignment" (routine? square-anon))
(test-check "Anonymous routine execution" (= (square-anon 6) 36))

;; Test 2: Anonymous routine used directly in expression
(displayln "--- Test 2: Direct anonymous routine usage ---")
(test-check "Direct anonymous routine call" (= ((routine (x) (* x x)) 7) 49))
(test-check "Direct anonymous with complex expr" (= ((routine (x y) (+ (* x x) (* y y))) 3 4) 25))

;; Test 3: Anonymous routine passed as argument
(routine apply-twice (f x) (f (f x)))

(displayln "--- Test 3: Anonymous routine as argument ---")
(test-check "Anonymous as function argument" 
            (= (apply-twice (routine (x) (+ x 1)) 10) 12))
(test-check "Anonymous multiplier as argument"
            (= (apply-twice (routine (x) (* x 2)) 5) 20))

;; Test 4: Anonymous routine in let binding
(displayln "--- Test 4: Anonymous routine in let binding ---")
(let ([cube (routine (x) (* x x x))])
  (test-check "Anonymous in let binding" (= (cube 4) 64)))

;; Test 5: Anonymous routine with yield in direct use
(displayln "--- Test 5: Anonymous with yield, direct use ---")
(test-check "Direct anonymous with yield" (= ((routine (x) (yield x) "done") 42) 42))

;; Test 6: Anonymous routine with yield as argument
(routine test-yielding-routine (r)
  (define first-result (r))
  (define second-result (r))
  (+ first-result second-result))

(displayln "--- Test 6: Anonymous with yield as argument ---")
(define yielding-anon (new (routine () (yield 10) 20)))
(test-check "Anonymous yielding routine as argument" 
            (= (test-yielding-routine yielding-anon) 30))

;; Test 7: Nested anonymous routines
(displayln "--- Test 7: Nested anonymous routines ---")
(define outer-routine 
  (routine (x)
    (define inner (routine (y) (* y 2)))
    (inner x)))
(test-check "Nested anonymous routine" (= (outer-routine 8) 16))

;; Test 8: Anonymous routine returning anonymous routine
(displayln "--- Test 8: Anonymous returning anonymous ---")
(define make-multiplier
  (routine (factor)
    (routine (x) (* x factor))))

(define double-maker (new make-multiplier 2))
(define doubler (double-maker))
(test-check "Anonymous returning anonymous" (= (doubler 9) 18))

;; Test 9: Anonymous routine in conditional
(displayln "--- Test 9: Anonymous in conditional ---")
(define choose-operation
  (routine (op)
    (if (equal? op 'add)
        (routine (x y) (+ x y))
        (routine (x y) (* x y)))))

(define adder-maker (new choose-operation 'add))
(define adder (adder-maker))
(test-check "Anonymous in conditional - add" (= (adder 5 7) 12))

(define mult-maker (new choose-operation 'mult))
(define multiplier (mult-maker))
(test-check "Anonymous in conditional - multiply" (= (multiplier 5 7) 35))

;; Test 10: Anonymous routine with complex closure
(displayln "--- Test 10: Anonymous with closure ---")
(define outer-var 100)
(define closure-test
  (routine (x)
    (define inner-var 50)
    (routine (y) (+ x y outer-var inner-var))))

(define closure-maker (new closure-test 10))
(define closure-routine (closure-maker))
(test-check "Anonymous with closure" (= (closure-routine 5) 165)) ; 10+5+100+50

;; Test 11: Anonymous routine in loop/iteration
(displayln "--- Test 11: Anonymous in iteration ---")
(define results '())
(for ([i '(1 2 3)])
  (define temp-routine (routine (x) (* x i)))
  (set! results (cons (temp-routine 10) results)))
(test-check "Anonymous in loop" (equal? (reverse results) '(10 20 30)))

;; Test 12: Anonymous routine with multiple yields
(displayln "--- Test 12: Anonymous with multiple yields ---")
(define multi-yield-anon
  (routine (start)
    (yield start)
    (yield (* start 2))
    (* start 3)))

(define multi-inst (new multi-yield-anon 6))
(test-check "Multi-yield anonymous - first" (= (multi-inst) 6))
(test-check "Multi-yield anonymous - second" (= (multi-inst) 12))
(test-check "Multi-yield anonymous - final" (= (multi-inst) 18))

;; Test 13: Anonymous routine composition
(displayln "--- Test 13: Anonymous routine composition ---")
(routine compose (f g)
  (routine (x) (f (g x))))

(define add-one (routine (x) (+ x 1)))
(define square (routine (x) (* x x)))
(define composed (compose add-one square))

(test-check "Anonymous composition" (= (composed 5) 26)) ; (5^2) + 1 = 26

;; Test 14: Anonymous routine with variable capture
(displayln "--- Test 14: Anonymous with variable capture ---")
(define make-counter
  (routine (initial)
    (define count initial)
    (routine ()
      (set! count (+ count 1))
      count)))

(define counter-maker (new make-counter 0))
(define counter (counter-maker))
(test-check "Anonymous counter - first call" (= (counter) 1))
(test-check "Anonymous counter - second call" (= (counter) 2))
(test-check "Anonymous counter - third call" (= (counter) 3))

;; Test 15: Anonymous routine with recursive-like behavior
(displayln "--- Test 15: Anonymous with recursive pattern ---")
(define make-factorial-like
  (routine (n)
    (if (<= n 1)
        (routine () 1)
        (routine () (* n 1))))) ; Simplified for testing

(define fact-maker (new make-factorial-like 5))
(define fact-routine (fact-maker))
(test-check "Anonymous recursive pattern" (= (fact-routine) 5))

(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All anonymous routine edge case tests passed!")
    (displayln "💥 Some anonymous routine edge case tests failed!"))
(displayln "=======================================================")
(displayln "")