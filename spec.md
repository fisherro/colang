# Colang

A programming language where every routine is a coroutine.

Built using Racket.

Eventually, I plan to transition to a more EcmaScript-style syntax. For now,
however, I'm using S-expressions.

## Spec notation

In this spec...

* All upper-case identifiers represent a placeholder
* Identifiers followed by `...` means "zero or more"

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
  (while #t
    (let-values (((new-x new-y) (yield y x)))
        (set! x new-x)
        (set! y new-y)
        (values x y))))
```

## Yield

`(yield VALUE...)`

Suspends the current coroutine yielding the given values. When the coroutine
resumes, this expression returns the values passed to the next activation.

## Resumable predicate

`(resumable? INSTANCE)`

Returns `#t` if the given instance is resumable; `#f` otherwise.

Note that `resumable?` returns true even if the routine hasn't had its first
activation yet. (Arguably, `callable?` might be a better name.)

## Construction

`(new COROUTINE ARGS...)`

Creates and returns a new instance of the given coroutine with the specified
arguments. The arguments are stored with the instance and will be used when
the coroutine is first activated.

If fewer arguments are provided than the coroutine expects, the remaining
arguments must be provided during the first activation.

### Construction Examples

```racket
; Provide all arguments during construction
(define one-to-ten (new iota 10))
(while (resumable? one-to-ten)
       (displayln (one-to-ten)))
```

```racket
(define (prefix-printer prefix message)
  (while #t
    (display prefix)
    (display ": ")
    (displayln message)
    (set! message (yield))))
(define print-it (new printer))
(print-it "Hello")
(print-it "World")
```

```racket
; Provide no arguments during construction
(define print-it (new printer))
(print-it "Hello")
(print-it "World")
```

## Activation

`(INSTANCE ARGS...)`

Activates the given instance of a coroutine passing the given arguments.

If this is the first activation of the instance, the arguments are combined
with any arguments that were stored during instance construction. The combined
arguments are bound to the routine's parameters, and execution starts at the
beginning of the routine.

If this is a resumption (the instance has yielded before), the arguments are
returned by the `yield` expression that suspended the instance.

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
