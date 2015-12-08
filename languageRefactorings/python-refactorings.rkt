#lang racket
(require syntax/parse)
(require syntax/to-string)
;;;;List of Python Refactoring Rules

(define (write-python sexp)
  (void))
(define racket-parser
  (syntax-parser
     #:datum-literals (:False :True expr-stmt)
    [(cond ((py-truth (py-lt arg arg2)) (expr-stmt :False)) (else (expr-stmt :True))) 
                 (write-python (string-append "not(" (syntax->string #'(arg)) "<" (syntax->string #'(arg2)) ")" ))]))