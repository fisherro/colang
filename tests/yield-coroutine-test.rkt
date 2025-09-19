#lang colang

;; Tests for yield behavior and coroutine lifecycle
;; Tests yield expressions, resumable predicates, and coroutine state management

(require "test-utils.rkt")
(include "colang-test-helpers.rkt")

(test-case "Basic yield behavior"
  (routine simple-yield (x)
    (yield x)
    (* x 2))
  
  ; Test with instance - should yield first, then return final result
  (define inst (new simple-yield 5))
  (check-resumable? inst)
  (check-equal? (inst) 5)  ; First yield
  (check-resumable? inst)
  (check-equal? (inst) 10) ; Final result
  (check-not-resumable? inst))

(test-case "Multiple yields"
  (routine counter (n)
    (define i 0)
    (while (< i n)
      (yield i)
      (set! i (+ i 1)))
    "done")
  
  ; Test counter that yields 0, 1, 2, then returns "done"
  (check-routine-yields counter (3) '(0 1 2) "done"))

(test-case "Yield with multiple values"
  (routine swap-values (x y)
    (yield y x)  ; Yield swapped values
    (values x y)) ; Return original order
  
  (define inst (new swap-values 10 20))
  (check-resumable? inst)
  (let-values (((a b) (inst)))
    (check-equal? a 20)
    (check-equal? b 10))
  (check-resumable? inst)
  (let-values (((c d) (inst)))
    (check-equal? c 10)
    (check-equal? d 20))
  (check-not-resumable? inst))

(test-case "Yield receives values from activation"
  (routine echo-routine ()
    (define msg (yield "ready"))
    (yield (string-append "Echo: " msg))
    "finished")
  
  (define inst (new echo-routine))
  (check-equal? (inst) "ready")
  (check-equal? (inst "hello") "Echo: hello")
  (check-equal? (inst) "finished")
  (check-not-resumable? inst))

(test-case "Resumable predicate lifecycle"
  (routine simple-counter (max)
    (define i 0)
    (while (< i max)
      (yield i)
      (set! i (+ i 1)))
    max)
  
  (define inst (new simple-counter 2))
  
  ; Before first activation
  (check-resumable? inst)
  
  ; After first activation (yielded 0)
  (check-equal? (inst) 0)
  (check-resumable? inst)
  
  ; After second activation (yielded 1)
  (check-equal? (inst) 1)
  (check-resumable? inst)
  
  ; After third activation (returned 2, finished)
  (check-equal? (inst) 2)
  (check-not-resumable? inst))

(test-case "Nested routine calls with yield"
  (routine inner-routine (x)
    (yield (* x 2))
    (* x 3))
  
  (routine outer-routine (y)
    (define result (inner-routine y)) ; Quick activation - gets first yield
    (yield result)
    (+ result 10))
  
  (define inst (new outer-routine 5))
  (check-equal? (inst) 10)  ; inner-routine yielded 5*2=10
  (check-equal? (inst) 20)  ; outer returned 10+10=20
  (check-not-resumable? inst))

(test-case "Yield in conditional branches"
  (routine conditional-yield (flag)
    (if flag
        (yield "true-branch")
        (yield "false-branch"))
    "finished")
  
  (define inst-true (new conditional-yield #t))
  (check-equal? (inst-true) "true-branch")
  (check-equal? (inst-true) "finished")
  
  (define inst-false (new conditional-yield #f))
  (check-equal? (inst-false) "false-branch")
  (check-equal? (inst-false) "finished"))

(test-case "Yield with no arguments"
  (routine yield-nothing ()
    (yield)
    "done")
  
  (define inst (new yield-nothing))
  (check-equal? (inst) (void))  ; yield with no args returns void
  (check-equal? (inst) "done")
  (check-not-resumable? inst))