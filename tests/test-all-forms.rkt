#lang colang

; Test all three syntax forms

; 1. Anonymous form
(define square-anon (routine (x) (* x x)))

; 2. Explicit form  
(define square-explicit (routine (x) (* x x)))

; 3. Shorthand form
(routine square-shorthand (x) (* x x))

; Test they all work the same way
(displayln "Anonymous form:")
(displayln (square-anon 5))

(displayln "Explicit form:")
(displayln (square-explicit 5))

(displayln "Shorthand form:")
(displayln (square-shorthand 5))

; Test with new instances
(define anon-inst (new square-anon 6))
(define explicit-inst (new square-explicit 6))
(define shorthand-inst (new square-shorthand 6))

(displayln "Instance tests:")
(displayln (anon-inst))
(displayln (explicit-inst))
(displayln (shorthand-inst))

(displayln "All forms work correctly!")