#lang racket
(provide write-processing)
(require syntax/parse)
(define (write-processing aux)
  (define (parse-processing-to-racket aux)
    ;;;;TO FIX BUGS
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt not p-lt p-le p-gt p-ge p-add)
      [(p-not stuff) #`(! #,@(parse-processing-to-racket #'stuff)) ]
      [(p-truth arg) #`(#,@(parse-processing-to-racket #'arg)) ] ;;call with arg
      [(p-lt arg1 arg2) #'(arg1 < arg2)]
      [(p-le arg1 arg2) #'(arg1 <= arg2)]
      [(p-gt arg1 arg2) #'(arg1 > arg2)]
      [(p-ge arg1 arg2) #'(#,@(parse-processing-to-racket #'arg1) >= #,@(parse-processing-to-racket #'arg2))]
      [(p-add expr1 expr2) #`(#,@(parse-processing-to-racket #'expr1) #'+ (parse-processing-to-racket #'expr1))]
      [(p-function (name arg ...) body) #`(int name {#,@(parse-processing-to-racket #'body)})]
      
      [_ (displayln "fail")]))
  (displayln "in write-processing")
  (parse-processing-to-racket aux))


;(p-function (fib-I-fn n) (let/ec return (p-block (p-if (p-lt-eq n 0) (return 0)) (p-if (p-eq n 1) (return 1)) (return (p-add (p-call #:call fib-I-fn (p-sub n 1)) (p-call #:call fib-I-fn (p-sub n 2))))))) (p-initialize)))>