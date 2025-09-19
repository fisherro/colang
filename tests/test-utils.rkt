#lang racket

;; Test utilities for Colang test suite
;; This module provides utilities for testing Colang routines and coroutines

(require rackunit)

(provide (all-defined-out))

;; Re-export rackunit so colang tests can use it
(provide (all-from-out rackunit))

;; Utility to capture output for testing display functions
(define (with-output-to-string* thunk)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (thunk))
  (get-output-string out))

;; Test that a thunk produces expected output
(define (check-output thunk expected-output)
  (check-equal? (with-output-to-string* thunk) expected-output))