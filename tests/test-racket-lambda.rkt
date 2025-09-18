#lang colang

; The ability to use the shorthand lambda definition from Racket should not
; leak through to Colang code.
(define (foo) (displayln 'foo))
(if (procedure? foo)
  (displayln "Whoops!")
  (displayln "Nothing to see here...move along!"))

; But we do expect `define` to work for values.
(define bar 'bar)
(if (symbol? bar)
  (displayln "Nothing to see here...move along!")
  (displayln "Whoops!"))

(define baz (routine (x) x))
(if (routine? baz)
  (displayln "Nothing to see here...move along!")
  (displayln "Whoops!"))

; We'd also like to block `lambda`
(define quux (lambda (x) x))
(if (procedure? quux)
  (displayln "Whoops!")
  (displayln "Nothing to see here...move along!"))
