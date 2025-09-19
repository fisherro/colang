#lang colang

;; Resume Arguments Test Suite
;; Testing complex resume argument patterns in Colang

(require "test-utils.rkt")

(displayln "=======================================================")
(displayln "             Resume Arguments Suite")
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

(displayln "Testing resume argument patterns...")

;; Test 1: Basic resume with argument
(displayln "--- Test 1: Basic resume with argument ---")
(routine basic-resume-test (x)
  (yield x)
  (yield (+ x 10)))

(define basic-instance (new basic-resume-test 5))
(test-check "Basic resume - first yield" (= (basic-instance) 5))
(test-check "Basic resume - second yield" (= (basic-instance) 15))

;; Test 2: Resume with different argument types
(displayln "--- Test 2: Resume with different argument types ---")
(routine type-test (initial)
  (define result (yield initial))
  (if (number? result)
      (yield (* result 2))
      (yield (string-append result "!"))))

(define type-instance (new type-test "hello"))
(test-check "Type test - initial string" (equal? (type-instance) "hello"))
(test-check "Type test - number resume" (= (type-instance 42) 84))

(define type-instance2 (new type-test 10))
(test-check "Type test 2 - initial number" (= (type-instance2) 10))
(test-check "Type test 2 - string resume" (equal? (type-instance2 "world") "world!"))

;; Test 3: Resume with multiple arguments
(displayln "--- Test 3: Resume with multiple arguments ---")
(routine multi-arg-test ()
  (define-values (a b) (yield 1 2))
  (yield (+ a b))
  (define-values (x y z) (yield 10 20 30))
  (+ x y z))

(define multi-instance (new multi-arg-test))
(test-check "Multi-arg - initial values" 
            (let-values (((a b) (multi-instance)))
              (and (= a 1) (= b 2))))
(test-check "Multi-arg - resume with 2 values" (= (multi-instance 3 4) 7))
(test-check "Multi-arg - final values" 
            (let-values (((x y z) (multi-instance)))
              (and (= x 10) (= y 20) (= z 30))))
(test-check "Multi-arg - resume with 3 values" (= (multi-instance 5 6 7) 18))

;; Test 4: Resume argument validation
(displayln "--- Test 4: Resume argument validation ---")
(routine validation-test (expected-type)
  (define input (yield "waiting"))
  (cond
    [(and (eq? expected-type 'number) (number? input)) 
     (yield (* input input))]
    [(and (eq? expected-type 'string) (string? input))
     (yield (string-length input))]
    [else (yield "invalid type")]))

(define num-validator (new validation-test 'number))
(test-check "Validation - waiting" (equal? (num-validator) "waiting"))
(test-check "Validation - valid number" (= (num-validator 7) 49))

(define str-validator (new validation-test 'string))
(test-check "Validation - waiting" (equal? (str-validator) "waiting"))
(test-check "Validation - valid string" (= (str-validator "hello") 5))

(define invalid-validator (new validation-test 'string))
(test-check "Invalid - waiting" (equal? (invalid-validator) "waiting"))
(test-check "Validation - invalid type" (equal? (invalid-validator 123) "invalid type"))

;; Test 5: Chained resume operations
(displayln "--- Test 5: Chained resume operations ---")
(routine chain-test (start)
  (define step1 (yield start))
  (define step2 (yield (+ step1 10)))
  (define step3 (yield (* step2 2)))
  (/ step3 4))

(define chain-instance (new chain-test 5))
(test-check "Chain - step 1" (= (chain-instance) 5))
(test-check "Chain - step 2" (= (chain-instance 1) 11))
(test-check "Chain - step 3" (= (chain-instance 3) 6))
(test-check "Chain - final" (= (chain-instance 8) 2))

;; Test 6: Resume with complex data structures
(displayln "--- Test 6: Resume with complex data structures ---")
(routine struct-test ()
  (define input-list (yield '(initial)))
  (define processed (if (equal? input-list '(1 2 3)) '(2 4 6) input-list))
  (yield processed)
  (define input-hash (yield #hash()))
  (hash-ref input-hash 'result "not found"))

(define struct-instance (new struct-test))
(test-check "Struct - initial list" (equal? (struct-instance) '(initial)))
(test-check "Struct - processed list" (equal? (struct-instance '(1 2 3)) '(2 4 6)))
(test-check "Struct - initial hash" (equal? (struct-instance) #hash()))
(test-check "Struct - hash lookup" 
            (equal? (struct-instance #hash((result . "found!"))) "found!"))

;; Test 7: Resume in nested contexts
(displayln "--- Test 7: Resume in nested contexts ---")
(routine outer-resume (x)
  (define inner-routine 
    (routine (y)
      (define z (yield y))
      (* z 3)))
  
  (define inner-instance (new inner-routine x))
  (define first (inner-instance))
  (yield first)
  (define second (inner-instance 10))
  (+ first second))

(define nested-instance (new outer-resume 5))
(test-check "Nested - outer yield" (= (nested-instance) 5))
(test-check "Nested - final result" (= (nested-instance) 35))

;; Test 8: Resume with conditional logic
(displayln "--- Test 8: Resume with conditional logic ---")
(routine conditional-resume (mode)
  (define input (yield "ready"))
  (cond
    [(eq? mode 'double) (yield (* input 2)) "doubled"]
    [(eq? mode 'square) (yield (* input input)) "squared"]
    [(eq? mode 'negate) (yield (- input)) "negated"]
    [else (yield input) "unchanged"]))

(define doubler (new conditional-resume 'double))
(test-check "Conditional - double ready" (equal? (doubler) "ready"))
(test-check "Conditional - double result" (= (doubler 6) 12))
(test-check "Conditional - double status" (equal? (doubler) "doubled"))

(define squarer (new conditional-resume 'square))
(test-check "Conditional - square ready" (equal? (squarer) "ready"))
(test-check "Conditional - square result" (= (squarer 5) 25))
(test-check "Conditional - square status" (equal? (squarer) "squared"))

;; Test 9: Resume with accumulation
(displayln "--- Test 9: Resume with accumulation ---")
(routine accumulator (initial)
  (define total initial)
  (let loop ()
    (define input (yield total))
    (if (eq? input 'stop)
        total
        (begin
          (set! total (+ total input))
          (loop)))))

(define acc-instance (new accumulator 10))
(test-check "Accumulator - initial" (= (acc-instance) 10))
(test-check "Accumulator - add 5" (= (acc-instance 5) 15))
(test-check "Accumulator - add 3" (= (acc-instance 3) 18))
(test-check "Accumulator - add 7" (= (acc-instance 7) 25))
(test-check "Accumulator - stop" (= (acc-instance 'stop) 25))

;; Test 10: Resume with error handling
(displayln "--- Test 10: Resume with error handling ---")
(routine safe-division (x)
  (define y (yield "enter divisor"))
  (if (= y 0)
      (yield "error: division by zero")
      (yield (/ x y)))
  "complete")

(define div-instance (new safe-division 20))
(test-check "Safe division - prompt" (equal? (div-instance) "enter divisor"))
(test-check "Safe division - valid" (= (div-instance 4) 5))
(test-check "Safe division - complete" (equal? (div-instance) "complete"))

(define div-instance2 (new safe-division 15))
(test-check "Safe division 2 - prompt" (equal? (div-instance2) "enter divisor"))
(test-check "Safe division 2 - error" (equal? (div-instance2 0) "error: division by zero"))
(test-check "Safe division 2 - complete" (equal? (div-instance2) "complete"))

;; Test 11: Resume with variable capture
(displayln "--- Test 11: Resume with variable capture ---")
(routine capture-test (multiplier)
  (define input (yield "ready for input"))
  (yield (* input multiplier))
  (define new-multiplier (yield "ready for new multiplier"))
  (set! multiplier new-multiplier)
  (define second-input (yield "ready for second input"))
  (* second-input multiplier))

(define capture-instance (new capture-test 3))
(test-check "Capture - ready" (equal? (capture-instance) "ready for input"))
(test-check "Capture - first transform" (= (capture-instance 4) 12))
(test-check "Capture - new multiplier prompt" 
            (equal? (capture-instance) "ready for new multiplier"))
(test-check "Capture - second input prompt" 
            (equal? (capture-instance 5) "ready for second input"))
(test-check "Capture - second transform" (= (capture-instance 6) 30))

;; Test 12: Resume with recursive pattern
(displayln "--- Test 12: Resume with recursive pattern ---")
(routine fibonacci-generator ()
  (yield 0)
  (yield 1)
  (let loop ([a 0] [b 1])
    (define next (+ a b))
    (yield next)
    (loop b next)))

(define fib-instance (new fibonacci-generator))
(test-check "Fibonacci - 0" (= (fib-instance) 0))
(test-check "Fibonacci - 1" (= (fib-instance) 1))
(test-check "Fibonacci - 1" (= (fib-instance) 1))
(test-check "Fibonacci - 2" (= (fib-instance) 2))
(test-check "Fibonacci - 3" (= (fib-instance) 3))
(test-check "Fibonacci - 5" (= (fib-instance) 5))
(test-check "Fibonacci - 8" (= (fib-instance) 8))

;; Display test summary
(displayln "")
(displayln "=======================================================")
(displayln (string-append "Tests completed: " (number->string (+ passed failed))))
(displayln (string-append "Passed: " (number->string passed)))
(displayln (string-append "Failed: " (number->string failed)))
(if (= failed 0)
    (displayln "🎉 All resume argument tests passed!")
    (displayln "💥 Some resume argument tests failed!"))
(displayln "=======================================================")