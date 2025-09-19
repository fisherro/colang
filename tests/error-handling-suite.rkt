#lang racket

;; Error handling tests for Colang syntax restrictions
;; These tests verify that invalid syntax is properly rejected

(provide main)

(define (test-error-case description filename expected-error-pattern)
  (printf "Testing: ~a~n" description)
  (define command (format "racket error-cases/~a 2>&1" filename))
  (define output (with-output-to-string
                   (lambda ()
                     (system command))))
  
  (cond
    [(regexp-match expected-error-pattern output)
     (printf "✅ ~a: Correctly rejected with expected error~n" description)
     #t]
    [(regexp-match #rx"Command exited with code 0" output)
     (printf "❌ ~a: Should have failed but succeeded~n" description)
     #f]
    [else
     (printf "❌ ~a: Failed but with unexpected error:~n" description)
     (printf "   Expected pattern: ~a~n" expected-error-pattern)
     (printf "   Actual output: ~a~n" (string-trim output))
     #f]))

(define (test-valid-case description code-string expected-result)
  (printf "Testing: ~a~n" description)
  (define temp-file "temp-valid-test.rkt")
  (with-output-to-file temp-file 
    (lambda () (printf "~a" code-string))
    #:exists 'replace)
  
  (define command (format "racket ~a 2>&1" temp-file))
  (define output (string-trim (with-output-to-string
                               (lambda ()
                                 (system command)))))
  
  (delete-file temp-file)
  
  (cond
    [(equal? output expected-result)
     (printf "✅ ~a: Works correctly~n" description)
     #t]
    [else
     (printf "❌ ~a: Output '~a', Expected: '~a'~n" 
             description output expected-result)
     #f]))

(define (main . args)
  (printf "~n")
  (printf "=======================================================~n")
  (printf "               Error Handling Tests~n")
  (printf "=======================================================~n")
  (printf "~n")
  
  (printf "Testing that Colang properly rejects invalid syntax...~n")
  (printf "~n")
  
  (define error-tests
    (list
     (list "Function definition should be blocked"
           "blocked-function-def.rkt"
           #rx"Function definitions not allowed")
     (list "Lambda expressions should be blocked"
           "blocked-lambda.rkt"
           #rx"Lambda expressions not allowed")
     (list "Old-style function syntax should be blocked"
           "old-syntax.rkt"
           #rx"Function definitions not allowed")))
  
  (define error-results
    (for/list ([test-info error-tests])
      (apply test-error-case test-info)))
  
  (printf "~n")
  (printf "Testing that valid Colang syntax still works...~n")
  (printf "~n")
  
  (define valid-tests
    (list
     (list "Valid routine definition"
           "#lang colang\n(routine test-routine (x) (* x x))\n(displayln (test-routine 5))"
           "25")
     (list "Valid value definition"
           "#lang colang\n(define x 42)\n(displayln x)"
           "42")
     (list "Valid explicit routine definition"
           "#lang colang\n(define square (routine (x) (* x x)))\n(displayln (square 6))"
           "36")))
  
  (define valid-results
    (for/list ([test-info valid-tests])
      (apply test-valid-case test-info)))
  
  (printf "~n")
  (printf "=======================================================~n")
  (define total-tests (+ (length error-tests) (length valid-tests)))
  (define passed-tests (+ (length (filter identity error-results))
                          (length (filter identity valid-results))))
  (define failed-tests (- total-tests passed-tests))
  
  (printf "Tests completed: ~a~n" total-tests)
  (printf "Passed: ~a~n" passed-tests)
  (printf "Failed: ~a~n" failed-tests)
  
  (if (= failed-tests 0)
      (printf "🎉 All error handling tests passed!~n")
      (printf "💥 Some error handling tests failed!~n"))
  (printf "=======================================================~n")
  (printf "~n"))

;; If this file is run directly, execute main
(module+ main
  (main))