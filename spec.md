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

Colang provides three ways to define routines:

### Anonymous routine

`(routine (PARAMS...) BODY)`

Creates an anonymous coroutine that takes the given parameters and has the given body. Returns a procedure that can be called directly or used with `new`.

### Explicit definition

`(define NAME (routine (PARAMS...) BODY))`

Defines a named coroutine by binding an anonymous routine to a variable.

### Shorthand definition

`(routine NAME (PARAMS...) BODY)`

Shorthand for defining a named coroutine. This is the recommended syntax for defining named routines as it works in all contexts.

The value of the last expression in `BODY` is the return value of the routine.
(In the future we may have an explicit `return` statement instead.) A routine
may return multiple values.

Routines are lexically scoped and close over their lexical environment.

### Examples

```racket
; Shorthand definition (recommended)
(routine square (x)
  (* x x))
```

```racket
; Shorthand definition
(routine iota (n)
  (define i 1)
  (while (< i n)
         (yield i)
         (set! i (+ i 1)))
  n)
```

```racket
; Shorthand definition with yield
(routine printer (message)
  (while #t
         (displayln message)
         (set! message (yield))))
```

```racket
; Shorthand definition
(routine swapper (x y)
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

`(new ROUTINE ARGS...)`

Creates and returns a new instance of the given routine with the specified
arguments. The routine can be either a named routine or an anonymous routine.
The arguments are stored with the instance and will be used when
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

`(ROUTINE ARGS...)`

This is equivalent to...

`((new ROUTINE) ARGS...)`

A new, temporary instance of the routine need not actually be created, but
the behavior should be the same as if it had been. This works with both
named routines and anonymous routines.

Note that if the routine yields, then, when using quick activation, it will
return the first value yielded and never run to completion.

## Scheduling

There is no scheduling logic. Routines pass control directly between each other
through activations and yielding.

## Topics to cover in the future

* Destructing assignment
* Error handling
* What happens when a finished instance is activated?
* Lifecycle: Any finalizing opportunity for coroutines?

## Syntax Evolution

### Current S-expression syntax

The current syntax uses three forms for routine definition:

* `(routine (PARAMS...) BODY)` - Anonymous routine
* `(define NAME (routine (PARAMS...) BODY))` - Explicit definition  
* `(routine NAME (PARAMS...) BODY)` - Shorthand definition

### Future EcmaScript-style syntax

Eventually, the language will transition to a more familiar syntax:

* `routine(PARAMS...) { BODY }` - Anonymous routine
* `define NAME = routine(PARAMS...) { BODY }` - Explicit definition
* `routine NAME(PARAMS...) { BODY }` - Shorthand definition

### Deprecated syntax

The old `(define (NAME PARAMS...) BODY)` syntax is no longer supported for
creating coroutines. It now creates regular Racket functions instead.
