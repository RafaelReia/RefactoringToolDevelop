#lang racket
(require syntax/parse)
(require syntax/to-string)
(require "python-pretty-pritting.rkt")
(provide python-parser)
;;;;List of Python Refactoring Rules

#;(define (write-python sexp)
  (displayln sexp))

(define (python-parser arg)
   (displayln "inside python-parser")
  (displayln arg)
  (define return (void))
  (syntax-parser
     #:datum-literals (:False :True expr-stmt if py-truth)
    [(cond ((py-truth (py-lt arg arg2)) (expr-stmt :False)) (else (expr-stmt :True)))
                 (write-python #'(not (py-truth (py-lt arg arg2))))
                 (set! return (string-append "not(" (syntax->string #'(arg)) " < " (syntax->string #'(arg2)) ")" ))]
    [(if (py-truth expr) :True :False) (write-python #'(if (py-truth #'expr)))]
    
    ;;(if (py-truth (py-lt 1 2)) :True :False) to (py-truth (py-lt 1 2)) to (1 < 2)
    #;[(if (py-turth (py-lt arg arg2)) :True :False)
     (write-python (string-append "(" (syntax->string #'(arg)) " < " (syntax->string #'(arg2)) ")" ))]
    ;;;;(list py-turth cond)  !!!!
    [_ (void)])
  return)