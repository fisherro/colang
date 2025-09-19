#lang colang

;; Tests for construction and activation patterns
;; Tests 'new' construction, quick activation, and instance activation

(require "test-utils.rkt")
(include "colang-test-helpers.rkt")

(test-case "Basic construction with new"
  (routine square (x) (* x x))
  
  (define inst (new square 5))
  (check-resumable? inst)
  (check-equal? (inst) 25)
  (check-not-resumable? inst))

(test-case "Construction with all arguments"
  (routine add-three (x y z) (+ x y z))
  
  (define inst (new add-three 1 2 3))
  (check-equal? (inst) 6)
  (check-not-resumable? inst))

(test-case "Construction with partial arguments"
  (routine multiply-three (x y z) (* x y z))
  
  ; Provide only first argument during construction
  (define inst (new multiply-three 2))
  (check-resumable? inst)
  
  ; Provide remaining arguments during first activation
  (check-equal? (inst 3 4) 24)
  (check-not-resumable? inst))

(test-case "Construction with no arguments"
  (routine get-pi () 3.14159)
  
  (define inst (new get-pi))
  (check-equal? (inst) 3.14159)
  (check-not-resumable? inst))

(test-case "Quick activation equivalence"
  (routine double (x) (* x 2))
  
  ; Quick activation should be equivalent to (new routine) followed by activation
  (check-equal? (double 7) 14)
  (check-equal? ((new double) 7) 14)
  (check-equal? (double 7) ((new double) 7)))

(test-case "Quick activation with yielding routine"
  (routine yielding-routine (x)
    (yield (* x 2))
    (* x 3))
  
  ; Quick activation should return first yield, not final result
  (check-quick-activation yielding-routine (5) 10)
  
  ; Compare with full instance activation
  (define inst (new yielding-routine 5))
  (check-equal? (inst) 10)   ; First yield
  (check-equal? (inst) 15)   ; Final result
  (check-not-resumable? inst))

(test-case "Quick activation with non-yielding routine"
  (routine simple-routine (x) (* x x))
  
  ; Quick activation should return final result since there's no yield
  (check-equal? (simple-routine 6) 36))

(test-case "Multiple instances of same routine"
  (routine counter (max)
    (define i 0)
    (while (< i max)
      (yield i)
      (set! i (+ i 1)))
    "done")
  
  ; Create multiple independent instances
  (define inst1 (new counter 2))
  (define inst2 (new counter 3))
  
  ; They should operate independently
  (check-equal? (inst1) 0)
  (check-equal? (inst2) 0)
  (check-equal? (inst1) 1)
  (check-equal? (inst2) 1)
  (check-equal? (inst1) "done")
  (check-equal? (inst2) 2)
  (check-not-resumable? inst1)
  (check-equal? (inst2) "done")
  (check-not-resumable? inst2))

(test-case "Arguments during activation"
  (routine message-processor ()
    (define msg (yield "ready"))
    (string-append "Processed: " msg))
  
  (define inst (new message-processor))
  (check-equal? (inst) "ready")
  (check-equal? (inst "hello") "Processed: hello")
  (check-not-resumable? inst))

(test-case "Mixed construction and activation arguments"
  (routine formatter (prefix)
    (define suffix (yield "waiting"))
    (string-append prefix "-" suffix))
  
  ; Provide prefix during construction
  (define inst (new formatter "LOG"))
  (check-equal? (inst) "waiting")
  
  ; Provide suffix during activation
  (check-equal? (inst "ERROR") "LOG-ERROR")
  (check-not-resumable? inst))

(test-case "Anonymous routine construction"
  (define anon-routine (routine (x) (* x x x)))
  
  (define inst (new anon-routine 3))
  (check-equal? (inst) 27)
  (check-not-resumable? inst))

(test-case "Construction with complex arguments"
  (routine list-processor (lst)
    (for ([item lst])
      (yield item))
    "processed")
  
  (define inst (new list-processor '(a b c)))
  (check-equal? (inst) 'a)
  (check-equal? (inst) 'b)
  (check-equal? (inst) 'c)
  (check-equal? (inst) "processed")
  (check-not-resumable? inst))