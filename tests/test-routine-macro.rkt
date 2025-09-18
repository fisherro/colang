#lang colang

; Test anonymous routine creation
(displayln "Testing anonymous routine...")

; Simple anonymous routine
(define square-routine (routine (x) (* x x)))

; Test direct call (quick activation)
(displayln (square-routine 5))  ; Should print 25

; Test with new and explicit instance
(define square-inst (new square-routine 7))
(displayln (square-inst))  ; Should print 49

; Test anonymous routine with yield
(define counter-routine 
  (routine (start)
    (define i start)
    (while #t
      (yield i)
      (set! i (+ i 1)))))

; Test the counter routine
(define counter (new counter-routine 10))
(displayln (counter))  ; Should print 10
(displayln (counter))  ; Should print 11
(displayln (counter))  ; Should print 12

; Test explicit define form
(define iota-v2 (routine (n)
  (define i 1)
  (while (< i n)
    (yield i)
    (set! i (+ i 1)))
  n))

; Test the explicit define form
(define nums (new iota-v2 5))
(displayln "iota-v2 output:")
(while (resumable? nums)
  (displayln (nums)))

(displayln "All tests completed!")