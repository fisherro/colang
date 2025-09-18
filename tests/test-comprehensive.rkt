#lang colang

(routine square (x)
  (* x x))

(routine counter (n)
  (define i 0)
  (while (< i n)
    (yield i)
    (set! i (+ i 1)))
  "done counting")

(displayln "=== Quick activation with non-yielding routine ===")
(displayln (square 7))

(displayln "=== Quick activation with yielding routine ===")
(displayln (counter 3))

(displayln "=== Instance with yielding routine ===")
(define c (new counter 3))
(displayln (c))
(displayln (c))
(displayln (c))
(displayln (c))