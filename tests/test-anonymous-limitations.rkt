#lang colang

(displayln "=== Testing anonymous routines in various contexts ===")

; Test 1: Anonymous routine assigned to variable (should work)
(define anon1 (routine (x) (* x x)))
(displayln "Test 1 - assigned to variable:")
(displayln (anon1 5))

; Test 2: Anonymous routine used directly in expression (potential limitation)
(displayln "Test 2 - used directly in expression:")
(displayln ((routine (x) (* x x)) 6))

; Test 3: Anonymous routine passed as argument (potential limitation)
(define apply-twice (routine (f x) (f (f x))))
(displayln "Test 3 - passed as argument:")
(displayln (apply-twice (routine (x) (+ x 1)) 10))

; Test 4: Anonymous routine in let binding
(displayln "Test 4 - in let binding:")
(let ([square (routine (x) (* x x))])
  (displayln (square 7)))

; Test 5: Anonymous routine with yield in various contexts
(displayln "Test 5 - anonymous with yield, direct use:")
(displayln ((routine (x) (yield x) "done") 42))

(displayln "Test 6 - anonymous with yield, as argument:")
(define test-routine (routine (r) (r) (r)))
(define yielding-routine (routine () (yield "first") "second"))
(define inst (new test-routine yielding-routine))
(displayln (inst))