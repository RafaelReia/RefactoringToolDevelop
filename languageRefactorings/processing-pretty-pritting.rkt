#lang racket
(provide write-processing)
(require syntax/parse)
(define (write-processing aux)
  (define (parse-processing-to-racket aux)
    ;;;;TO IMPLEMENT
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt not py-lt py-add)
      [(not stuff) #`(not #,@(parse-processing-to-racket #'stuff)) ]
      [(py-truth arg) #`(#,@(parse-processing-to-racket #'arg)) ] ;;call with arg
      [(py-lt arg1 arg2) #'(arg1 < arg2)]
      [(py-le arg1 arg2) #'(arg1 <= arg2)]
      [(py-gt arg1 arg2) #'(arg1 > arg2)]
      [(py-ge arg1 arg2) #'(#,@(parse-processing-to-racket #'arg1) >= #,@(parse-processing-to-racket #'arg2))]
      [(py-add expr1 expr2) #`(#,@(parse-processing-to-racket #'expr1) #'+ (parse-processing-to-racket #'expr1))]
      [_ (displayln "fail")]))
  (displayln "in write-processing")
  (parse-processing-to-racket aux))