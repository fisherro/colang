#lang colang

#|
--------------------------------------------------------------------------------
(define (NAME PARAMS...) BODY)

Defines a coroutine named NAME that takes the given parametrs and has given
body.

--------------------------------------------------------------------------------
(yield VALUE...)

Suspends the current coroutine yielding the given values. When the coroutine
resumes, this expression yields the values passed from the routine that resumed
this one.

--------------------------------------------------------------------------------
(new COROUTINE)

Creates and returns a new instance of the given coroutine that is ready for its
first activation.

--------------------------------------------------------------------------------
(INSTANCE ARGS...)

Activates the given instance of a coroutine passing the given arguments. If
this is the first activation of the instance, it starts at the beginning of the
body and runs until it yields a value or returns a value. This expression then
returns that value.

--------------------------------------------------------------------------------
(COROUTINE ARGS...)

This is equivalent to...

((new COROUTINE) ARGS...)

A new, temporary instance of the coroutine need not actually be created, but
the behavior should be the same as if it had been.

|#

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
