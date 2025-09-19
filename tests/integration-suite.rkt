#lang colang

;; Comprehensive integration tests for complex Colang scenarios

(displayln "=======================================================")
(displayln "                 Integration Tests")
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

(displayln "Testing complex scenarios and integration...")
(displayln "")

;; Test 1: Producer-consumer pattern
(routine producer (items)
  (for ([item items])
    (yield item))
  'producer-done)

(define prod (new producer '(1 2 3 4)))
(define collected '())

; Consume all items
(while (resumable? prod)
  (define item (prod))
  (unless (equal? item 'producer-done)
    (set! collected (cons item collected))))

(test-check "Producer-consumer pattern" (equal? (reverse collected) '(1 2 3 4)))

;; Test 2: Fibonacci generator
(routine fibonacci-gen ()
  (define a 0)
  (define b 1)
  (yield a)
  (yield b)
  (define count 2)
  (while (< count 8) ; Generate first 8 fibonacci numbers
    (define next (+ a b))
    (yield next)
    (set! a b)
    (set! b next)
    (set! count (+ count 1)))
  'fib-done)

(define fib (new fibonacci-gen))
(define fib-sequence '())
(while (resumable? fib)
  (define next-fib (fib))
  (unless (equal? next-fib 'fib-done)
    (set! fib-sequence (cons next-fib fib-sequence))))

(test-check "Fibonacci sequence generation" 
            (equal? (reverse fib-sequence) '(0 1 1 2 3 5 8 13)))

;; Test 3: State machine simulation
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
      [(equal? input 'stop)
       (set! state 'idle)])))

(define machine (new state-machine 'idle))
(test-check "State machine - initial state" (equal? (machine) 'idle))
(test-check "State machine - start transition" (equal? (machine 'start) 'running))
(test-check "State machine - pause transition" (equal? (machine 'pause) 'paused))
(test-check "State machine - resume transition" (equal? (machine 'resume) 'running))
(test-check "State machine - stop transition" (equal? (machine 'stop) 'idle))

;; Test 4: Pipeline processing
(routine number-source (max)
  (define i 1)
  (while (<= i max)
    (yield i)
    (set! i (+ i 1)))
  'source-done)

(routine filter-evens (source)
  (while (resumable? source)
    (define value (source))
    (unless (equal? value 'source-done)
      (when (even? value)
        (yield value))))
  'filter-done)

(routine square-values (source)
  (while (resumable? source)
    (define value (source))
    (unless (equal? value 'filter-done)
      (yield (* value value))))
  'square-done)

; Build pipeline: numbers(1-6) -> filter evens -> square
(define source (new number-source 6))
(define filtered (new filter-evens source))
(define squared (new square-values filtered))

; Collect results
(define results '())
(while (resumable? squared)
  (define result (squared))
  (unless (equal? result 'square-done)
    (set! results (cons result results))))

(test-check "Pipeline processing" (equal? (reverse results) '(4 16 36))) ; squares of 2, 4, 6

;; Test 5: Accumulator pattern
(routine accumulator (initial)
  (define sum initial)
  (while #t
    (define value (yield sum))
    (set! sum (+ sum value))))

(define acc (new accumulator 10))
(test-check "Accumulator initial" (= (acc) 10))
(test-check "Accumulator add 5" (= (acc 5) 15))
(test-check "Accumulator add 3" (= (acc 3) 18))
(test-check "Accumulator subtract 8" (= (acc -8) 10))

;; Test 6: Coroutine communication
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

; Alternate between ping and pong
(while (or (resumable? ping) (resumable? pong))
  (when (resumable? ping)
    (define ping-msg (ping))
    (unless (equal? ping-msg "ping-done")
      (set! conversation (cons ping-msg conversation))))
  (when (resumable? pong)
    (define pong-msg (pong))
    (unless (equal? pong-msg "pong-done")
      (set! conversation (cons pong-msg conversation)))))

(test-check "Coroutine communication pattern"
            (equal? (reverse conversation) 
                    '("ping-1" "pong-1" "ping-2" "pong-2" "ping-3" "pong-3")))

;; Test 7: Nested routine interactions
(routine inner-yielder (x)
  (yield (* x 2))
  (* x 3))

(routine outer-caller (y)
  (define result (inner-yielder y)) ; Quick activation - gets first yield
  (yield result)
  (+ result 10))

(define outer-inst (new outer-caller 5))
(test-check "Nested routine first yield" (= (outer-inst) 10)) ; inner yielded 5*2=10
(test-check "Nested routine final result" (= (outer-inst) 20)) ; outer returned 10+10=20

(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All integration tests passed!")
    (displayln "💥 Some integration tests failed!"))
(displayln "=======================================================")
(displayln "")