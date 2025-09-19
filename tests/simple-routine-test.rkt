#lang colang

;; Simple test for routine definitions to validate our approach
;; This will show output to verify tests are working

(require "test-utils.rkt")

(displayln "=== Running Routine Definition Tests ===")

;; Test anonymous routine
(define square-anon (routine (x) (* x x)))
(if (routine? square-anon)
    (displayln "✅ Anonymous routine: routine? predicate works")
    (displayln "❌ Anonymous routine: routine? predicate failed"))

(if (= (square-anon 5) 25)
    (displayln "✅ Anonymous routine: computation works")
    (displayln "❌ Anonymous routine: computation failed"))

;; Test explicit routine
(define square-explicit (routine (x) (* x x)))
(if (routine? square-explicit)
    (displayln "✅ Explicit routine: routine? predicate works")
    (displayln "❌ Explicit routine: routine? predicate failed"))

(if (= (square-explicit 6) 36)
    (displayln "✅ Explicit routine: computation works")
    (displayln "❌ Explicit routine: computation failed"))

;; Test shorthand routine
(routine square-shorthand (x) (* x x))
(if (routine? square-shorthand)
    (displayln "✅ Shorthand routine: routine? predicate works")
    (displayln "❌ Shorthand routine: routine? predicate failed"))

(if (= (square-shorthand 7) 49)
    (displayln "✅ Shorthand routine: computation works")
    (displayln "❌ Shorthand routine: computation failed"))

;; Test that all forms are equivalent
(if (and (= (square-anon 8) (square-explicit 8))
         (= (square-explicit 8) (square-shorthand 8)))
    (displayln "✅ All routine forms produce equivalent results")
    (displayln "❌ Routine forms produce different results"))

(displayln "=== Routine Definition Tests Complete ===")
(displayln "")