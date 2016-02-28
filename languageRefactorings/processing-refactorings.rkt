#lang racket
(require syntax/parse)
(require syntax/to-string)
(require "processing-pretty-pritting.rkt")
(provide python-parser)
;;;;List of Processing Refactoring Rules


(define (python-parser arg)
   (displayln "inside processing-parser")
  (displayln arg)
  (define return (void))
  (syntax-parse arg
     #:datum-literals (:False :True expr-stmt if py-truth)
    [(cond ((py-truth (py-lt arg arg2)) (expr-stmt :False)) (else (expr-stmt :True)))
     (begin
       (write-processing #'(not (py-truth (py-lt arg arg2))))
       (set! return (string-append "not(" (syntax->string #'(arg)) " < " (syntax->string #'(arg2)) ")" )))]
    [(expr-stmt (if (py-truth expr) :True :False))
          (begin (displayln "here2")
     (set! return (write-processing #'(if (py-truth #'expr)))))]
    
    ;;(if (py-truth (py-lt 1 2)) :True :False) to (py-truth (py-lt 1 2)) to (1 < 2)
    #;[(if (py-turth (py-lt arg arg2)) :True :False)
     (displayln "fail 2")]
    ;;;;(list py-turth cond)  !!!!
    [_ (begin (displayln "none :(" )(void))])
  return)