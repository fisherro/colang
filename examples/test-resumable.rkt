#lang colang

(define test-coroutine (routine (n)
  (displayln "Started")
  (yield n)
  (displayln "Resumed")
  "done"))

(displayln "=== Testing resumable? behavior ===")

; Test fresh instance
(define cor (new test-coroutine))
(displayln (string-append "Fresh instance resumable? " (if (resumable? cor) "TRUE" "FALSE")))

; Test after first activation
(displayln "First activation:")
(displayln (cor 42))
(displayln (string-append "After first activation resumable? " (if (resumable? cor) "TRUE" "FALSE")))

; Test after resume
(displayln "Resume:")
(displayln (cor))
(displayln (string-append "After completion resumable? " (if (resumable? cor) "TRUE" "FALSE")))