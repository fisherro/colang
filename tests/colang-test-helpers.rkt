;; Colang-specific test utilities
;; Include this file in each test to get access to testing helpers

;; Test that a routine produces expected output when called directly
(define-syntax-rule (check-routine-output routine args expected)
  (check-equal? (routine . args) expected))

;; Test that a routine yields expected values in sequence
(routine check-routine-yields (routine args expected-yields final-result)
  (define inst (apply new routine args))
  
  ;; Check each yield
  (for ([expected expected-yields])
    (check-true (resumable? inst) "Instance should be resumable")
    (check-equal? (inst) expected))
  
  ;; Check final result
  (check-true (resumable? inst) "Instance should be resumable for final call")
  (check-equal? (inst) final-result)
  
  ;; Check that instance is no longer resumable
  (check-false (resumable? inst) "Instance should not be resumable after completion"))

;; Test that quick activation returns first yield (not final result)
(routine check-quick-activation (routine args expected-first-yield)
  (check-equal? (apply routine args) expected-first-yield))

;; Test that a value is a routine
(define-syntax-rule (check-routine? value)
  (check-true (routine? value) (format "~a should be a routine" 'value)))

;; Test that a value is not a routine
(define-syntax-rule (check-not-routine? value)
  (check-false (routine? value) (format "~a should not be a routine" 'value)))

;; Test that an instance is resumable
(define-syntax-rule (check-resumable? instance)
  (check-true (resumable? instance) (format "~a should be resumable" 'instance)))

;; Test that an instance is not resumable
(define-syntax-rule (check-not-resumable? instance)
  (check-false (resumable? instance) (format "~a should not be resumable" 'instance)))