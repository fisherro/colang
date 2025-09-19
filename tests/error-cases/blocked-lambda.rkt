#lang colang

;; This file should fail with a lambda expression error
(define forbidden-lambda (lambda (x) x))