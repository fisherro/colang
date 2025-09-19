#lang colang

;; This file should fail with a function definition error
(define (forbidden-function x) (* x x))