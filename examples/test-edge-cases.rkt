#lang colang

; Test resume with arguments (should pass to yield)
(define (test-resume-args initial)
  (displayln (string-append "Started with: " (number->string initial)))
  (define resume-value (yield initial))
  (displayln (string-append "Resumed with: " (number->string resume-value)))
  (+ initial resume-value))

(displayln "=== Testing resume arguments ===")
(define test-instance (new test-resume-args 10))
(displayln "First activation:")
(displayln (test-instance))
(displayln "Resume with argument 20:")
(displayln (test-instance 20))

; Test multiple parameters
(define (test-multiple a b c)
  (displayln (string-append "a=" (number->string a) " b=" (number->string b) " c=" (number->string c)))
  (yield (+ a b c))
  (* a b c))

(displayln "")
(displayln "=== Testing multiple parameters ===")
(define multi-instance (new test-multiple 2 3 4))
(displayln (multi-instance))
(displayln (multi-instance))