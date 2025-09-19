#lang colang

;; Comprehensive test suite for Colang yield and coroutine behavior

(displayln "=======================================================")
(displayln "             Yield and Coroutine Tests")
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

(displayln "Testing yield and coroutine behavior...")
(displayln "")

;; Test 1: Basic yield behavior
(routine simple-yield (x)
  (yield x)
  (* x 2))

(define inst1 (new simple-yield 5))
(test-check "Fresh instance is resumable" (resumable? inst1))
(test-check "First activation returns yield value" (= (inst1) 5))
(test-check "After yield, still resumable" (resumable? inst1))
(test-check "Second activation returns final value" (= (inst1) 10))
(test-check "After completion, not resumable" (not (resumable? inst1)))

;; Test 2: Multiple yields
(routine counter (n)
  (define i 0)
  (while (< i n)
    (yield i)
    (set! i (+ i 1)))
  "done")

(define counter-inst (new counter 3))
(test-check "Counter first yield" (= (counter-inst) 0))
(test-check "Counter second yield" (= (counter-inst) 1))
(test-check "Counter third yield" (= (counter-inst) 2))
(test-check "Counter final result" (equal? (counter-inst) "done"))
(test-check "Counter finished" (not (resumable? counter-inst)))

;; Test 3: Yield receives values from activation
(routine echo-routine ()
  (define msg (yield "ready"))
  (string-append "Echo: " msg))

(define echo-inst (new echo-routine))
(test-check "Echo initial yield" (equal? (echo-inst) "ready"))
(test-check "Echo with input" (equal? (echo-inst "hello") "Echo: hello"))
(test-check "Echo completed" (not (resumable? echo-inst)))

;; Test 4: Quick activation with yielding routine
(routine yielding-for-quick (x)
  (yield (* x 2))
  (* x 3))

(test-check "Quick activation returns first yield" (= (yielding-for-quick 4) 8))

;; Test 5: Quick activation vs instance activation
(define normal-inst (new yielding-for-quick 4))
(test-check "Instance first activation matches quick activation" (= (normal-inst) 8))
(test-check "Instance second activation gives final result" (= (normal-inst) 12))

;; Test 6: Multiple instances are independent
(routine independent-counter (max)
  (define i 0)
  (while (< i max)
    (yield i)
    (set! i (+ i 1)))
  "finished")

(define inst-a (new independent-counter 2))
(define inst-b (new independent-counter 2))
(test-check "Instance A first yield" (= (inst-a) 0))
(test-check "Instance B first yield" (= (inst-b) 0))
(test-check "Instance A second yield" (= (inst-a) 1))
(test-check "Instance B second yield" (= (inst-b) 1))
(test-check "Instance A finished" (equal? (inst-a) "finished"))
(test-check "Instance B finished" (equal? (inst-b) "finished"))

;; Test 7: Yield with no arguments
(routine yield-nothing ()
  (yield)
  "done")

(define void-inst (new yield-nothing))
; Note: (void) comparison might be tricky, so we just test that it runs
(void-inst) ; Call the yield
(test-check "Yield with no args completes" (equal? (void-inst) "done"))

(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All yield and coroutine tests passed!")
    (displayln "💥 Some yield and coroutine tests failed!"))
(displayln "=======================================================")
(displayln "")