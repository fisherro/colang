#lang colang

(define (square x)
  (* x x))

(define (iota n)
  (define i 1)
  (while (< i n)
         (yield i)
         (set! i (+ i 1)))
  n)

(define (printer message)
  (while #t
         (displayln message)
         (set! message (yield))))

(define (swapper x y)
  (while #t
    (let-values (((new-x new-y) (yield y x)))
        (set! x new-x)
        (set! y new-y)
        (values x y))))

(displayln "==== square test ====")
(displayln (square 5))

(displayln "==== iota test ====")
(define one-to-ten (new iota))
(one-to-ten 10)
(while (resumable? one-to-ten)
       (displayln (one-to-ten)))

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
