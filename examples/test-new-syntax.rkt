#lang colang

; Test that old syntax fails
; (define (old-style x) (* x x))  ; This should fail if uncommented

; Test that new syntax still works
(define square-new (routine (x) (* x x)))
(displayln (square-new 6))  ; Should print 36

; Test explicit define form
(define iota-new (routine (n)
  (define i 1)
  (while (< i n)
    (yield i)
    (set! i (+ i 1)))
  n))

(define nums (new iota-new 4))
(while (resumable? nums)
  (displayln (nums)))

(displayln "New syntax working correctly!")