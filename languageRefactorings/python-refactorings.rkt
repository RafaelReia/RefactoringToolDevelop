#lang racket
(require syntax/parse)
(require syntax/to-string)
(require "python-pretty-pritting.rkt")
(provide python-parser)
;;;;List of Python Refactoring Rules

#;(define (write-python sexp)
  (displayln sexp))
;;; TODO accept expr-stmt
(define (python-parser arg)
   (displayln "inside python-parser")
  (displayln arg)
  (define return (void))
  (syntax-parse arg
     #:datum-literals (:False :True expr-stmt if py-not py-truth :alpha :beta cond)
    [(cond ((py-truth (py-lt arg arg2)) (expr-stmt :False)) (else (expr-stmt :True)))
     (begin
       (write-python #'(py-not (py-lt arg arg2)))
       #;(set! return (string-append "not(" (syntax->string #'(arg)) " < " (syntax->string #'(arg2)) ")" )))]
    #;[(expr-stmt (if (py-truth (py-lt :alpha :beta)) :True :False))
          (begin (displayln "here2")
     (set! return #'(alpha < beta)))]

    [(expr-stmt (if (py-truth expr) :True :False))
          (begin (displayln "here2")
     (set! return (write-python #'expr)))]
    
    ;;(if (py-truth (py-lt 1 2)) :True :False) to (py-truth (py-lt 1 2)) to (1 < 2)
    #;[(if (py-turth (py-lt arg arg2)) :True :False)
     (displayln "fail 2")]
    ;;;;(list py-turth cond)  !!!!
    [_ (begin (displayln "none :(")(void))])
  return)