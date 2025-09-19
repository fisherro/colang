#lang colang

;; Tests for routine? and resumable? predicates
;; Tests predicate behavior with various types of values

(require "test-utils.rkt")
(include "colang-test-helpers.rkt")

(test-case "routine? predicate with various routine forms"
  ; Test with shorthand definition
  (routine shorthand-routine (x) (* x 2))
  (check-routine? shorthand-routine)
  
  ; Test with explicit definition
  (define explicit-routine (routine (x) (+ x 1)))
  (check-routine? explicit-routine)
  
  ; Test with anonymous routine
  (define anon-routine (routine (x) (- x 1)))
  (check-routine? anon-routine))

(test-case "routine? predicate with non-routines"
  ; Test with regular Racket functions
  (define regular-func (lambda (x) (/ x 2)))
  (check-not-routine? regular-func)
  
  ; Test with built-in functions
  (check-not-routine? +)
  (check-not-routine? cons)
  (check-not-routine? displayln)
  
  ; Test with primitive values
  (check-not-routine? 42)
  (check-not-routine? "hello")
  (check-not-routine? #t)
  (check-not-routine? #f)
  (check-not-routine? '(1 2 3))
  (check-not-routine? (void)))

(test-case "routine? predicate with instances"
  (routine test-routine (x) (* x x))
  
  ; routine? should return true for the routine itself
  (check-routine? test-routine)
  
  ; routine? should return false for instances
  (define inst (new test-routine 5))
  (check-not-routine? inst))

(test-case "resumable? predicate lifecycle"
  (routine yielding-routine (x)
    (yield (* x 2))
    (* x 3))
  
  (define inst (new yielding-routine 4))
  
  ; Fresh instance should be resumable
  (check-resumable? inst)
  
  ; After first activation (yielded), still resumable
  (check-equal? (inst) 8)
  (check-resumable? inst)
  
  ; After final activation (returned), no longer resumable
  (check-equal? (inst) 12)
  (check-not-resumable? inst))

(test-case "resumable? predicate with non-yielding routine"
  (routine simple-routine (x) (* x x))
  
  (define inst (new simple-routine 5))
  
  ; Fresh instance should be resumable
  (check-resumable? inst)
  
  ; After activation (no yield, just returned), no longer resumable
  (check-equal? (inst) 25)
  (check-not-resumable? inst))

(test-case "resumable? predicate with non-instances"
  (routine test-routine (x) x)
  
  ; resumable? should return false for the routine itself
  (check-not-resumable? test-routine)
  
  ; resumable? should return false for non-routine values
  (check-not-resumable? 42)
  (check-not-resumable? "hello")
  (check-not-resumable? (lambda (x) x))
  (check-not-resumable? '(1 2 3)))

(test-case "resumable? predicate with zero-parameter routine"
  (routine no-params ()
    (yield "middle")
    "end")
  
  (define inst (new no-params))
  (check-resumable? inst)
  (check-equal? (inst) "middle")
  (check-resumable? inst)
  (check-equal? (inst) "end")
  (check-not-resumable? inst))

(test-case "resumable? predicate with multiple yields"
  (routine multiple-yields (n)
    (define i 0)
    (while (< i n)
      (yield i)
      (set! i (+ i 1)))
    "finished")
  
  (define inst (new multiple-yields 3))
  
  ; Check resumable state through multiple yields
  (check-resumable? inst)
  (check-equal? (inst) 0)
  (check-resumable? inst)
  (check-equal? (inst) 1)
  (check-resumable? inst)
  (check-equal? (inst) 2)
  (check-resumable? inst)
  (check-equal? (inst) "finished")
  (check-not-resumable? inst))

(test-case "Predicates with routines that take multiple arguments"
  (routine multi-arg-routine (x y z)
    (yield (+ x y))
    (* x y z))
  
  (check-routine? multi-arg-routine)
  
  (define inst (new multi-arg-routine 2 3 4))
  (check-not-routine? inst)
  (check-resumable? inst)
  
  (check-equal? (inst) 5)
  (check-resumable? inst)
  
  (check-equal? (inst) 24)
  (check-not-resumable? inst))

(test-case "Predicates with anonymous routines"
  (define anon (routine (x) (yield x) (* x 2)))
  
  (check-routine? anon)
  
  (define inst (new anon 7))
  (check-not-routine? inst)
  (check-resumable? inst)
  
  (check-equal? (inst) 7)
  (check-resumable? inst)
  
  (check-equal? (inst) 14)
  (check-not-resumable? inst))