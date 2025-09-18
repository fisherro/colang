#lang colang

; Examples from the spec - demonstrating all three syntax forms

; Shorthand definition (from spec)
(routine square (x)
  (* x x))

; Shorthand definition (from spec) 
(routine iota (n)
  (define i 1)
  (while (< i n)
         (yield i)
         (set! i (+ i 1)))
  n)

; Shorthand definition with yield (from spec)
(routine printer (message)
  (while #t
         (displayln message)
         (set! message (yield))))

; Shorthand definition (from spec)
(routine swapper (x y)
  (while #t
    (let-values (((new-x new-y) (yield y x)))
        (set! x new-x)
        (set! y new-y)
        (values x y))))

; Test all the examples from the spec

(displayln "==== square test (shorthand syntax) ====")
(displayln (square 5))

(displayln "==== iota test (shorthand syntax) ====")
(define one-to-ten (new iota 10))
(while (resumable? one-to-ten)
       (displayln (one-to-ten)))

(displayln "==== printer test (shorthand syntax) ====")
(define print-it (new printer))
(print-it "Hello")
(print-it "World")

(displayln "==== swapper test (shorthand syntax) ====")
(define swap-it (new swapper))
(let-values (((a b) (swap-it 5 10)))
  (displayln a)
  (displayln b)
  (let-values (((c d) (swap-it 50 100)))
    (displayln c)
    (displayln d)))
