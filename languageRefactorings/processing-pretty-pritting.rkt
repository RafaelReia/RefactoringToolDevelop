#lang racket
(provide write-processing)
(require syntax/parse
         syntax/to-string)
(define (write-processing aux)
  (define (parse-processing-to-racket aux)
    ;;;;TO FIX BUGS
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt p-not p-truth p-lt p-le p-gt p-ge p-add
                               let/ec p-block p-if p-lt-eq return
                               p-call p-sub p-function p-eq)
      [(p-not stuff) #`(! #,@(parse-processing-to-racket #'stuff)) ]
      [(p-truth arg) #`(#,@(parse-processing-to-racket #'arg)) ] ;;call with arg
      [(p-lt arg1 arg2) #'(arg1 < arg2)]
      [(p-le arg1 arg2) #'(arg1 <= arg2)]
      [(p-gt arg1 arg2) #'(arg1 > arg2)]
      [(p-eq arg1 arg2) #'(arg1 == arg2)]
      [(p-ge arg1 arg2) #'(arg1 >= arg2)]
      [(p-declaration (name stuff)) #`(boolean name = #,@(parse-processing-to-racket #'stuff)) ] 
      ;[(p-ge arg1 arg2) #'(#,@(parse-processing-to-racket #'arg1) >= #,@(parse-processing-to-racket #'arg2))]
      ;[(p-add expr1 expr2) #`(#,@(parse-processing-to-racket #'expr1) #'+ (parse-processing-to-racket #'expr1))]
      [(p-function (name arg ...) body)
       #`(#,(parse-name #'name) (int arg ...)
                                 {#,@(parse-processing-to-racket #'body)})]
      [(let/ec return stuff) (parse-processing-to-racket #'stuff)]
      [(p-block stuff stuff2 stuff3) #`(#,@(parse-processing-to-racket #'stuff) #,@(parse-processing-to-racket #'stuff2) #,@(parse-processing-to-racket #'stuff3))]
      [(p-if expr result) #`(if #,(parse-processing-to-racket #'expr)
                             #,@(parse-processing-to-racket #'result))]
      [(p-lt-eq arg1 arg2) #'(arg1 <= arg2)]
      [(return (p-add arg1 arg2)) #`(return #,@(parse-processing-to-racket
                                         #'(p-add arg1 arg2))";")]
      [(return arg) #'(return arg";")]
      [(p-add arg1 arg2) #`(#,@(parse-processing-to-racket #'arg1) + 
                            #,@(parse-processing-to-racket #'arg2))]
      [(p-call #:call name arg) #`(#,(parse-name-call #'name) #,(parse-processing-to-racket #'arg))]
      [(p-sub arg1 arg2) #'(- arg1 arg2)]
      [_ (displayln "fail")]))
  (displayln "in write-processing")
  (parse-processing-to-racket aux))

(define (parse-name name)
  (define aux (syntax->string #`(#,name)))
  (define aux-lst (string-split aux "-"))
  (define return
    (case (cadr aux-lst)
    [("I") (string-append "int " (car aux-lst))]))
  #`(#,@return))
(define (parse-name-call name)
  (define aux (syntax->string #`(#,name)))
  (car (string-split aux "-")))

;(p-function (fib-I-fn n) (let/ec return
;(p-block (p-if (p-lt-eq n 0) (return 0))
;(p-if (p-eq n 1) (return 1))
;(return (p-add (p-call #:call fib-I-fn (p-sub n 1))
;(p-call #:call fib-I-fn (p-sub n 2))))))) (p-initialize)))>