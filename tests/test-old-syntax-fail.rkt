#lang colang

; This should fail with the old syntax
(define (old-style x) (* x x))
(displayln (old-style 5))