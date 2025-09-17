#lang s-exp syntax/module-reader
colang

;; This is a simple reader.rkt single we're currently using a simple S-expression syntax.
;; We'd like a EcmaScript-style syntax in the future, in which case this file will be more complex.

#| #lang racket/base
   (require syntax/module-reader)
   (provide (rename-out [coroutine-read read]
                        [coroutine-read-syntax read-syntax]))
   
   (define-values (coroutine-read coroutine-read-syntax)
     (make-meta-reader
      'colang
      "language path"
      (lambda (bstr)
        (read-syntax #f (open-input-bytes bstr)))
      (lambda (str)
     (read (open-input-string str))))) |#