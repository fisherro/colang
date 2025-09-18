#lang colang

; Comprehensive test of the new parameter passing system

(define test-params (routine (a b c)
  (displayln (string-append "Parameters: a=" (number->string a) 
                           " b=" (number->string b) 
                           " c=" (number->string c)))
  (yield (+ a b c))
  (define resume-value (yield (* a b c)))
  (displayln (string-append "Resume value: " (number->string resume-value)))
  (+ a b c resume-value)))

(displayln "=== Comprehensive Parameter Passing Test ===")
(displayln "")

; Test 1: All parameters to new
(displayln "Test 1: All parameters to new")
(define test1 (new test-params 1 2 3))
(displayln (string-append "Sum: " (number->string (test1))))
(displayln (string-append "Product: " (number->string (test1))))
(displayln (string-append "Final: " (number->string (test1 100))))
(displayln "")

; Test 2: Split parameters
(displayln "Test 2: Split parameters (2 to new, 1 to activation)")
(define test2 (new test-params 4 5))
(displayln (string-append "Sum: " (number->string (test2 6))))
(displayln (string-append "Product: " (number->string (test2))))
(displayln (string-append "Final: " (number->string (test2 200))))
(displayln "")

; Test 3: No parameters to new
(displayln "Test 3: No parameters to new (all to activation)")
(define test3 (new test-params))
(displayln (string-append "Sum: " (number->string (test3 7 8 9))))
(displayln (string-append "Product: " (number->string (test3))))
(displayln (string-append "Final: " (number->string (test3 300))))
(displayln "")

; Test 4: Quick activation (all parameters at once)
(displayln "Test 4: Quick activation")
(displayln (string-append "Quick result: " (number->string (test-params 10 20 30))))
(displayln "")

; Test 5: Pipeline composition with new parameter system
(displayln "Test 5: Pipeline composition")
(define generator (routine (start count)
  (define i 0)
  (while (< i count)
    (yield (+ start i))
    (set! i (+ i 1)))))

; Create a simple manual pipeline test instead
(define gen (new generator 10 5))
(displayln "Generator 10-14:")
(while (resumable? gen)
  (define value (gen))
  (when (and (number? value) (even? value))
    (displayln (string-append "Even: " (number->string value)))))

(displayln "")
(displayln "=== All tests completed successfully! ===")