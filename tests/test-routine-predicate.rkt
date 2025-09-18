#lang colang

(displayln "=== Testing routine? predicate ===")

; Test with various routine forms
(routine named-routine (x) (* x x))
(define explicit-routine (routine (x) (+ x 1)))
(define anon-routine (routine (x) (- x 1)))

; Test with non-routines
(define regular-func (lambda (x) (/ x 2)))
(define some-number 42)
(define some-string "hello")

; Test routine? predicate
(displayln "Named routine:")
(displayln (routine? named-routine))

(displayln "Explicit routine:")
(displayln (routine? explicit-routine))

(displayln "Anonymous routine:")
(displayln (routine? anon-routine))

(displayln "Regular function:")
(displayln (routine? regular-func))

(displayln "Number:")
(displayln (routine? some-number))

(displayln "String:")
(displayln (routine? some-string))

; Test with instances
(define inst (new named-routine 5))
(displayln "Coroutine instance:")
(displayln (routine? inst))

; Test that routines still work
(displayln "Calling named routine:")
(displayln (named-routine 6))