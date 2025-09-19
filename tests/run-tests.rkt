#!/usr/bin/env racket
#lang racket

;; Main test runner for Colang test suite
;; Runs all test files individually and provides a comprehensive report

(provide main)

;; Test file information
(define test-files 
  '(("Routine Definitions" "routine-definitions-suite.rkt")
    ("Yield and Coroutines" "yield-coroutine-suite.rkt")
    ("Construction and Activation" "construction-activation-suite.rkt")
    ("Predicates" "predicates-suite.rkt")
    ("Integration Tests" "integration-suite.rkt")))

;; Function to run a single test file
(define (run-test-file name filename)
  (printf "Running ~a tests (~a)...~n" name filename)
  (define result 
    (system (format "racket ~a" filename)))
  (if result
      (begin
        (printf "✅ ~a: PASSED~n" name)
        #t)
      (begin
        (printf "❌ ~a: FAILED~n" name)
        #f)))

;; Main test runner function
(define (main . args)
  (printf "~n")
  (printf "=======================================================~n")
  (printf "              Colang Test Suite Runner                ~n") 
  (printf "=======================================================~n")
  (printf "~n")
  
  (printf "Running comprehensive tests for Colang language...~n")
  (printf "~n")
  
  ;; Change to tests directory
  (define original-dir (current-directory))
  (current-directory (build-path original-dir "tests"))
  
  ;; Run each test file
  (define results
    (for/list ([test-info test-files])
      (define name (first test-info))
      (define filename (second test-info))
      (printf "~n")
      (run-test-file name filename)))
  
  ;; Restore original directory
  (current-directory original-dir)
  
  (printf "~n")
  (printf "=======================================================~n")
  (printf "                    Test Summary                      ~n")
  (printf "=======================================================~n")
  
  ;; Calculate summary
  (define total-tests (length results))
  (define passed-tests (length (filter identity results)))
  (define failed-tests (- total-tests passed-tests))
  
  ;; Print summary
  (if (= failed-tests 0)
      (begin
        (printf "✅ All test suites passed!~n")
        (printf "   Test suites run: ~a~n" total-tests)
        (printf "   Passed: ~a~n" passed-tests)
        (printf "   Failed: ~a~n" failed-tests))
      (begin
        (printf "❌ Some test suites failed!~n")
        (printf "   Test suites run: ~a~n" total-tests)
        (printf "   Passed: ~a~n" passed-tests)
        (printf "   Failed: ~a~n" failed-tests)))
  
  (printf "~n")
  (printf "Individual test files contain detailed RackUnit test results.~n")
  (printf "~n")
  
  ;; Return appropriate exit code
  (if (= failed-tests 0) 0 1))

;; If this file is run directly, execute main
(module+ main
  (main))