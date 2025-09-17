#lang colang

(define (square x)
  (* x x))

(define (iota n)
  (define i 0)
  (while (< i n)
         (yield i)
         (set! i (+ i 1))))

(define (printer message)
  (while #t
         (displayln message)
         (set! message (yield))))

(define (swapper x y)
  (let-values (((new-x new-y) (yield y x)))
    (set! x new-x)
    (set! y new-y)))

(displayln "==== square test ====")
(displayln (square 5))

(displayln "==== iota test ====")
(define zero-to-nine (new iota))
(iota 10)
(while (resumable? zero-to-nine)
       (displayln (zero-to-nine)))

(displayln "==== printer test ====")
(define print-it (new printer))
(print-it "Hello")
(print-it "World")

(displayln "==== swapper test ====")
(define swap-it (new swapper))
(let-values (((a b) (swap-it 5 10)))
  (displayln a)
  (displayln b)
  (let-values (((c d) (swap-it 50 100)))
    (displayln c)
    (displayln d)))
