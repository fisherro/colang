#lang colang

;; Comprehensive test suite for Colang routine definitions
;; Tests all three ways to define routines and their behavior

(displayln "=======================================================")
(displayln "             Routine Definition Tests")
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

(displayln "Testing routine definition forms...")
(displayln "")

;; Test 1: Anonymous routine definition
(define square-anon (routine (x) (* x x)))
(test-check "Anonymous routine is recognized as routine" (routine? square-anon))
(test-check "Anonymous routine computes correctly" (= (square-anon 5) 25))

;; Test 2: Explicit routine definition  
(define square-explicit (routine (x) (* x x)))
(test-check "Explicit routine is recognized as routine" (routine? square-explicit))
(test-check "Explicit routine computes correctly" (= (square-explicit 6) 36))

;; Test 3: Shorthand routine definition
(routine square-shorthand (x) (* x x))
(test-check "Shorthand routine is recognized as routine" (routine? square-shorthand))
(test-check "Shorthand routine computes correctly" (= (square-shorthand 7) 49))

;; Test 4: All forms are equivalent
(test-check "All routine forms produce equivalent results" 
            (and (= (square-anon 8) (square-explicit 8))
                 (= (square-explicit 8) (square-shorthand 8))))

;; Test 5: Multiple parameters
(routine add-three (x y z) (+ x y z))
(test-check "Multi-parameter routine works" (= (add-three 1 2 3) 6))

;; Test 6: No parameters
(routine get-answer () 42)
(test-check "Zero-parameter routine works" (= (get-answer) 42))

;; Test 7: Complex body with conditionals
(routine factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(test-check "Recursive routine works" (= (factorial 5) 120))

;; Test 8: Lexical scoping
(define outer-var 100)
(routine use-outer () outer-var)
(routine shadow-outer ()
  (define outer-var 200)
  outer-var)
(test-check "Lexical scoping - accessing outer variable" (= (use-outer) 100))
(test-check "Lexical scoping - shadowing outer variable" (= (shadow-outer) 200))
(test-check "Lexical scoping - outer variable unchanged" (= outer-var 100))

(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All routine definition tests passed!")
    (displayln "💥 Some routine definition tests failed!"))
(displayln "=======================================================")
(displayln "")