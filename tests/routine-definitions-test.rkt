#lang colang

;; Tests for routine definition forms
;; Tests the three ways to define routines: anonymous, explicit, and shorthand

(require "test-utils.rkt")
(include "colang-test-helpers.rkt")

;; Test all three routine definition forms
(test-case "Anonymous routine definition"
  ; Anonymous form: (routine (PARAMS...) BODY)
  (define square-anon (routine (x) (* x x)))
  
  (check-routine? square-anon)
  (check-routine-output square-anon (5) 25)
  (check-routine-output square-anon (7) 49))

(test-case "Explicit routine definition"
  ; Explicit form: (define NAME (routine (PARAMS...) BODY))
  (define square-explicit (routine (x) (* x x)))
  
  (check-routine? square-explicit)
  (check-routine-output square-explicit (5) 25)
  (check-routine-output square-explicit (7) 49))

(test-case "Shorthand routine definition"
  ; Shorthand form: (routine NAME (PARAMS...) BODY)
  (routine square-shorthand (x) (* x x))
  
  (check-routine? square-shorthand)
  (check-routine-output square-shorthand (5) 25)
  (check-routine-output square-shorthand (7) 49))

(test-case "All forms produce equivalent results"
  (define square-anon (routine (x) (* x x)))
  (define square-explicit (routine (x) (* x x)))
  (routine square-shorthand (x) (* x x))
  
  (check-equal? (square-anon 8) (square-explicit 8))
  (check-equal? (square-explicit 8) (square-shorthand 8))
  (check-equal? (square-anon 8) (square-shorthand 8)))

(test-case "Routines with multiple parameters"
  (routine add-three (x y z) (+ x y z))
  (define multiply-two (routine (x y) (* x y)))
  
  (check-routine-output add-three (1 2 3) 6)
  (check-routine-output multiply-two (4 5) 20))

(test-case "Routines with no parameters"
  (routine get-answer () 42)
  (define get-pi (routine () 3.14159))
  
  (check-routine-output get-answer () 42)
  (check-routine-output get-pi () 3.14159))

(test-case "Routines with complex bodies"
  (routine factorial (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))
  
  (check-routine-output factorial (5) 120)
  (check-routine-output factorial (0) 1)
  (check-routine-output factorial (1) 1))

(test-case "Routines with lexical scoping"
  (define x 10)
  (routine use-outer-x () x)
  (routine shadow-x ()
    (define x 20)
    x)
  
  (check-routine-output use-outer-x () 10)
  (check-routine-output shadow-x () 20)
  ; x should still be 10 in outer scope
  (check-equal? x 10))