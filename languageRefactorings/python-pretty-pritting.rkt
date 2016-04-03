#lang racket
(provide write-python)
(require syntax/parse)
(define (write-python aux)
  ;;; TODO accept expr-stmt
  (define (parse-python-to-racket aux)
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt py-not py-truth py-lt py-ge py-le py-gt py-add)
      [(py-not stuff) #`(not #,@(parse-python-to-racket #'stuff)) ]
      [(py-truth (py-lt arg1 arg2)) #'(arg1 < arg2)]
      [(py-truth arg) #'arg] ;;call with arg
      [(py-lt arg1 arg2) #'(arg1 < arg2)]
      [(py-le arg1 arg2) #'(arg1 <= arg2)]
      [(py-gt arg1 arg2) #'(arg1 > arg2)]
      [(py-ge arg1 arg2) #'(#,@(parse-python-to-racket #'arg1) >= #,@(parse-python-to-racket #'arg2))]
      [(py-add expr1 expr2) #`(#,@(parse-python-to-racket #'expr1) #'+ (parse-python-to-racket #'expr1))]
      [(expr-stmt (if (py-truth expr) :True :False)) #`(True if #,@(parse-python-to-racket #'expr) else False)]
      [_ (displayln "fail")]))
  (displayln "in write-python")
  (parse-python-to-racket aux))

;;;;
;; if -> if
;; (py-truth (stuff)) -> (stuff)
;; (py-lt args ...)  -> ((car arg) < (cdr arg))
;; 

#;(write-python #'(not (py-truth (py-lt 1 2))))



;;
;;;