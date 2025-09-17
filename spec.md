# Colang

A programming language where every routine is a coroutine.

Built using Racket.

Eventually, I plan to transition to a more EcmaScript-style syntax. For now,
however, I'm using S-expressions.

## Defining routines

`(define (NAME PARAMS...) BODY)`

Defines a coroutine named `NAME` that takes the given parameters and has given
body.

The value of the last expression in `BODY` is the return value of the routine.
(In the future we have have an explicit `return` statement instead.) A routine
may return multiple values.

Routines are lexically scoped and close over their lexical environment.

### Examples

```racket
(define (square x)
  (* x x))
```

```racket
(define (iota n)
  (define i 1)
  (while (< i n)
         (yield i)
         (set! i (+ i 1)))
  n)
```

```racket
(define (printer message)
  (while #t
         (displayln message)
         (set! message (yield))))
```

```racket
(define (swapper x y)
  (let-values (((new-x new-y) (yield y x)))
    (set! x new-x)
    (set! y new-y)))
```

## Yield

`(yield VALUE...)`

Suspends the current coroutine yielding the given values. When the coroutine
resumes, this expression returns the values passed to the next activation.

## Resumable predicate

`(resumable? INSTANCE)`

Returns `#t` if the given instance is resumable; `#f` otherwise.

## Construction

`(new COROUTINE)`

Creates and returns a new instance of the given coroutine that is ready for its
first activation.

### Example

```racket
(define one-to-ten (new iota))
(one-to-ten 10)
(while (resumable? one-to-ten)
       (displayln (one-to-ten)))
```

## Activation

`(INSTANCE ARGS...)`

Activates the given instance of a coroutine passing the given arguments.

If this is the first activation of the instance, it starts at the beginning
with the arguments bound to the parameters.

Otherwise, the arguments are returned by the `yield` that suspended the
instance.

## Quick activation

`(COROUTINE ARGS...)`

This is equivalent to...

`((new COROUTINE) ARGS...)`

A new, temporary instance of the coroutine need not actually be created, but
the behavior should be the same as if it had been.

## Scheduling

There is no scheduling logic. Routines pass control directly between each other
through activations and yielding.

## Topics to cover in the future

* Destructing assignment
* Error handling
* What happens when a finished instance is activated?
* Lifecycle: Any finalizing opportunity for coroutines?
