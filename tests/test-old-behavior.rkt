#lang colang

; This should work as regular function, not coroutine
(define (old-style x) (* x x))
(displayln (old-style 5))

; Test that it's NOT a coroutine (can't use with new)
; (define inst (new old-style 3))  ; This should fail if uncommented
; (displayln (inst))

; Test that yield doesn't work in old syntax
(define (old-with-yield x)
  (yield x)  ; This should fail
  (* x x))

; (displayln (old-with-yield 4))  ; This should fail if uncommented