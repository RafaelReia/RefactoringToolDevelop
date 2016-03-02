#lang racket
(require syntax/parse)
(provide create-meta-lang)
(define (refactoring-meta-lang arg)
  (void))

(define python #f)
(define processing #f)

(define (create-meta-lang aux)
  (displayln "%%%%%%%%%%%%%%%%%%%%%%%%%")
  (define return null)
  ;location information
  ;scope
  ;bindings
  ;stop case!!
  (define (python-parser aux)
    (displayln "^^^^^^^^")
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt py-not py-truth py-lt py-le py-ge py-gt py-add)
      [(py-not stuff) #`(not #,@(python-parser #'stuff)) ]
      [(py-truth arg) #`(#,@(python-parser #'arg)) ] ;;call with arg
      [(py-lt arg1 arg2) #`(< #,@(python-parser #'arg1) #,@(python-parser #'arg2))]
      [(py-le arg1 arg2) #`(<= #,@(python-parser #'arg1) #,@(python-parser #'arg2))]
      [(py-gt arg1 arg2) #`(> #,@(python-parser #'arg1) #,@(python-parser #'arg2))]
      [(py-ge arg1 arg2) #`(>= #,@(python-parser #'arg1) #,@(python-parser #'arg2))]
      #;[(py-add expr1 expr2) #`(#,@(python-parser #'expr1) #'+ (python-parser #'expr1))]
      ;[arg #'arg]
      [_ (begin (set! python #f) (displayln python)(displayln "fail"))]))
  
  (define (processing-parser aux)
    (displayln "^^^^^^^^")
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt p-not p-lt p-le p-gt p-ge p-add p-truth)
      [(p-not stuff) #`(! #,@(processing-parser #'stuff)) ]
      [(p-truth arg) #`(#,@(processing-parser #'arg)) ] ;;call with arg
      [(p-lt arg1 arg2) (set! return #'(< arg1 arg2))]
      [(p-le arg1 arg2) (set! return #`(<= #,@(processing-parser #'arg1) #,@(processing-parser #'arg2)))]
      [(p-gt arg1 arg2) #`(> #,@(processing-parser #'arg1) #,@(processing-parser #'arg2))]
      [(p-ge arg1 arg2) #`(>= #,@(processing-parser #'arg1) #,@(processing-parser #'arg2))]
      #;[(p-add expr1 expr2) #`(#,@(processing-parser #'expr1) #'+ (processing-parser #'expr1))]
      ;[arg #'arg]
      [_ (begin (set! processing #f) (displayln processing) (displayln "fail"))]))
  #;(set! python #t)
  #;(displayln "python-parser")
  (python-parser aux)
  (set! processing #t)
  (displayln "processing-parser")
  (processing-parser aux)
  (display "processing ")
  (displayln processing)
  (display "python ")
  (displayln python)
  (displayln return)
(displayln "%%%%%%%%%%%%%%%%%%%%%%%%%"))
