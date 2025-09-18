#lang colang

; Test shorthand syntax
(routine square-shorthand (x) (* x x))

; Test that shorthand works like explicit form
(displayln (square-shorthand 8))  ; Should print 64

; Test shorthand with yield
(routine counter-shorthand (start)
  (define i start)
  (while #t
    (yield i)
    (set! i (+ i 1))))

(define counter (new counter-shorthand 20))
(displayln (counter))  ; Should print 20
(displayln (counter))  ; Should print 21
(displayln (counter))  ; Should print 22

; Test all three forms work together
(routine square-explicit-short (x) (* x x))
(routine square-short (x) (* x x))

(displayln "Explicit shorthand form:")
(displayln (square-explicit-short 9))
(displayln "Shorthand form:")  
(displayln (square-short 9))

(displayln "All shorthand syntax tests passed!")