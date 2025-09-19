#lang colang

;; Integration tests for complex scenarios and edge cases
;; Tests complex interactions and edge cases from the specification

(require "test-utils.rkt")
(include "colang-test-helpers.rkt")

(test-case "Producer-consumer pattern"
  (routine producer (items)
    (for ([item items])
      (yield item))
    'producer-done)
  
  (routine consumer (prod-instance)
    (define results '())
    (while (resumable? prod-instance)
      (define item (prod-instance))
      (set! results (cons item results)))
    (reverse results))
  
  (define prod (new producer '(1 2 3 4)))
  (define result (consumer prod))
  (check-equal? result '(1 2 3 4)))

(test-case "Coroutine communication pattern"
  (routine ping-routine ()
    (define count 0)
    (while (< count 3)
      (set! count (+ count 1))
      (yield (string-append "ping-" (number->string count))))
    "ping-done")
  
  (routine pong-routine ()
    (define count 0)
    (while (< count 3)
      (set! count (+ count 1))
      (yield (string-append "pong-" (number->string count))))
    "pong-done")
  
  (define ping (new ping-routine))
  (define pong (new pong-routine))
  
  (define conversation '())
  (while (or (resumable? ping) (resumable? pong))
    (when (resumable? ping)
      (set! conversation (cons (ping) conversation)))
    (when (resumable? pong)
      (set! conversation (cons (pong) conversation))))
  
  (check-equal? (reverse conversation) 
                '("ping-1" "pong-1" "ping-2" "pong-2" "ping-3" "pong-3")))

(test-case "Fibonacci generator"
  (routine fibonacci-gen ()
    (define a 0)
    (define b 1)
    (yield a)
    (yield b)
    (while #t
      (define next (+ a b))
      (yield next)
      (set! a b)
      (set! b next)))
  
  (define fib (new fibonacci-gen))
  (check-equal? (fib) 0)
  (check-equal? (fib) 1)
  (check-equal? (fib) 1)
  (check-equal? (fib) 2)
  (check-equal? (fib) 3)
  (check-equal? (fib) 5)
  (check-equal? (fib) 8)
  (check-resumable? fib)) ; Should continue indefinitely

(test-case "State machine simulation"
  (routine state-machine (initial-state)
    (define state initial-state)
    (while #t
      (define input (yield state))
      (cond
        [(and (equal? state 'idle) (equal? input 'start))
         (set! state 'running)]
        [(and (equal? state 'running) (equal? input 'pause))
         (set! state 'paused)]
        [(and (equal? state 'paused) (equal? input 'resume))
         (set! state 'running)]
        [(and (or (equal? state 'running) (equal? state 'paused)) 
              (equal? input 'stop))
         (set! state 'idle)])))
  
  (define machine (new state-machine 'idle))
  (check-equal? (machine) 'idle)
  (check-equal? (machine 'start) 'running)
  (check-equal? (machine 'pause) 'paused)
  (check-equal? (machine 'resume) 'running)
  (check-equal? (machine 'stop) 'idle))

(test-case "Pipeline processing"
  (routine filter-evens (source)
    (while (resumable? source)
      (define value (source))
      (when (even? value)
        (yield value)))
    'filter-done)
  
  (routine square-values (source)
    (while (resumable? source)
      (define value (source))
      (yield (* value value)))
    'square-done)
  
  ; Create a number source
  (routine number-source (max)
    (define i 1)
    (while (<= i max)
      (yield i)
      (set! i (+ i 1)))
    'source-done)
  
  ; Build pipeline: numbers -> filter evens -> square
  (define source (new number-source 6))
  (define filtered (new filter-evens source))
  (define squared (new square-values filtered))
  
  ; Collect results
  (define results '())
  (while (resumable? squared)
    (set! results (cons (squared) results)))
  
  (check-equal? (reverse results) '(4 16 36))) ; squares of 2, 4, 6

(test-case "Recursive coroutine"
  (routine countdown (n)
    (if (<= n 0)
        'done
        (begin
          (yield n)
          (countdown (- n 1)))))
  
  ; Note: This tests quick activation with recursive calls
  (check-equal? (countdown 3) 3)  ; Should return first yield
  
  ; Test with instance to see full sequence
  (define cd (new countdown 3))
  (check-equal? (cd) 3)
  ; Note: Due to quick activation in recursive call, 
  ; the behavior might be different than expected)

(test-case "Error handling with yield"
  (routine error-prone-routine (should-error)
    (yield "before-error-check")
    (if should-error
        (error "Intentional error")
        (yield "no-error"))
    "completed")
  
  ; Test normal case
  (define normal-inst (new error-prone-routine #f))
  (check-equal? (normal-inst) "before-error-check")
  (check-equal? (normal-inst) "no-error")
  (check-equal? (normal-inst) "completed")
  
  ; Test error case
  (define error-inst (new error-prone-routine #t))
  (check-equal? (error-inst) "before-error-check")
  (check-exn exn:fail? (lambda () (error-inst))))

(test-case "Complex argument passing"
  (routine accumulator (initial)
    (define sum initial)
    (while #t
      (define value (yield sum))
      (set! sum (+ sum value))))
  
  (define acc (new accumulator 10))
  (check-equal? (acc) 10)      ; Initial value
  (check-equal? (acc 5) 15)    ; 10 + 5
  (check-equal? (acc 3) 18)    ; 15 + 3
  (check-equal? (acc -8) 10))  ; 18 - 8

(test-case "Multiple value yields and returns"
  (routine multi-value-routine (x y)
    (yield x y (+ x y))  ; Yield three values
    (values (* x 2) (* y 3)))  ; Return two values
  
  (define inst (new multi-value-routine 4 5))
  (let-values (((a b c) (inst)))
    (check-equal? a 4)
    (check-equal? b 5)
    (check-equal? c 9))
  
  (let-values (((d e) (inst)))
    (check-equal? d 8)
    (check-equal? e 15))
  
  (check-not-resumable? inst))