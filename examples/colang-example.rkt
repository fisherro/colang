#lang colang

(define (producer n)
  (let ((m 0))
    (while (> n 0)
           (set! m (yield (+ n m)))
           (set! n (- n 1)))))

(define (consumer)
  (displayln (producer 10)) ; returns 10
  (let ((cor (new producer))) ; constructs a producer instance
    (displayln (cor 3)) ; initial call
    (while (resumable? cor)
           (displayln (cor 10))))) ; resume the instance

(consumer)
