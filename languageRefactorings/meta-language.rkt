#lang racket
(require syntax/parse)
(provide create-meta-lang)
(provide meta-lang-parser)
(require "python-pretty-pritting.rkt")
(require "processing-pretty-pritting.rkt")
(define (meta-lang-parser arg)
  ;location information
  ;scope
  ;bindings
  ;stop case!!
  (write-back-meta-lang (refactoring-meta-lang (create-meta-lang arg)))) ;TO BE TESTED
(define (refactoring-meta-lang arg)
  (define return null)
  (syntax-parse arg
    [(not (> a b))
     (set! return #'(<= a b))]
    [(not (<= a b))
     (set! return #'(> a b))]
    [(not (< a b))
     (set! return #'(>= a b))]
    [(not (>= a b))
     (set! return #'(< a b))]
    [_ (set! return (void))])
  return)

(define python #f)
(define processing #f)
(define racket #f)
(define (create-meta-lang aux)
  (define return null)
  (define (python-parser aux)
    ;(define-py-function :fib with-params (n)
    ;(lambda (:n) (cond ((py-truth (py-eq :n 0)) 0) ((py-truth (py-eq :n 1)) 1) (else (py-add (py-call :fib (py-sub :n 1)) (py-call :fib (py-sub :n 2)))
    (displayln aux)
    (syntax-parse aux
      #:datum-literals (:False :True lambda expr-stmt py-not
                               py-eq py-truth py-lt py-le py-ge
                               py-gt py-add define-py-function
                               with-params cond py-call)
      [(define-py-function name with-params (n ...) body)
       (set! return #`(define (name n ...) #,(python-parser #'body)))] ;;small bug falta ':
      #;[(lambda (stuff) stuff2) (displayln #'stuff2)]
      [(lambda (stuff) (cond (case1 arg1) (case2 arg2) (else elsecase)))
       (set! return
             #`(cond (#,(python-parser #'case1) arg1)
                     (#,(python-parser #'case2) arg2)
                     (else #,(python-parser #'elsecase))))]
      [(py-truth expr)
       (set! return #`(#,@(python-parser #'expr)))]
      [(py-eq arg1 arg2) (set! return #'(eq? arg1 arg2))]
      [(py-call name arg) (set! return #`(name #,(python-parser #'arg)))]
      [(py-call name (arg)) (set! return #`(name #,@(python-parser #'arg)))]
      [(py-not stuff) #`(not #,@(python-parser #'stuff)) ]
      [(py-truth arg) #`(#,@(python-parser #'arg)) ] ;;call with arg
      [(py-lt arg1 arg2) #`(< #,@(python-parser #'arg1) #,@(python-parser #'arg2))]
      [(py-le arg1 arg2) #`(<= #,@(python-parser #'arg1) #,@(python-parser #'arg2))]
      [(py-gt arg1 arg2) #`(> #,@(python-parser #'arg1) #,@(python-parser #'arg2))]
      [(py-ge arg1 arg2) #`(>= #,@(python-parser #'arg1) #,@(python-parser #'arg2))]
      [(py-add expr1 expr2) (set! return #`(+ #,(python-parser #'expr1) #,(python-parser #'expr2)))]
      [(py-sub expr1 expr2) (set! return #'(- expr1 expr2))]
      ;[arg #'arg]
      [_ (begin (set! python #f)(displayln "fail"))])
    return)
  
  (define (processing-parser aux)
    (displayln aux)
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt p-not p-lt p-le p-gt p-ge p-add p-truth)
      [(p-not stuff) (set! return #`(not #,(processing-parser #'stuff)))]
      [(p-truth arg) #`(#,@(processing-parser #'arg)) ] ;;call with arg
      [(p-lt arg1 arg2) (set! return #'(< arg1 arg2))]
      [(p-le arg1 arg2) (set! return #`(<= #,@(processing-parser #'arg1) #,@(processing-parser #'arg2)))]
      [(p-gt arg1 arg2) #`(> #,@(processing-parser #'arg1) #,@(processing-parser #'arg2))]
      [(p-ge arg1 arg2) #`(>= #,@(processing-parser #'arg1) #,@(processing-parser #'arg2))]
      #;[(p-add expr1 expr2) #`(#,@(processing-parser #'expr1) #'+ (processing-parser #'expr1))]
      ;[arg #'arg]
      [_ (begin (set! processing #f) (displayln processing) (displayln "fail"))])
    return)
  (set! python #t)
  (displayln "python-parser")
  (python-parser aux)
  #;#;#;#;#;#;(set! processing #t)
  (displayln "processing-parser")
  (processing-parser aux)
  (display "processing ")
  (displayln processing)
  (display "python ")
  (displayln python)
  (displayln return)
  return)

(define (write-back-meta-lang arg)
  (define return null)
  (define (python-parser aux)
    (displayln "Write-back- Python")
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt py-not py-truth py-lt py-le py-ge py-gt py-add)
      [(not stuff) #`(py-not #,@(python-parser #'stuff))]
      ;[(py-truth arg) #`(#,@(python-parser #'arg)) ] ;;call with arg
      [(< arg1 arg2) (set! return (write-python #'(py-lt arg1 arg2)))] ;to be corrected
      [(<= arg1 arg2) (set! return (write-python #'(py-le 'arg1arg2)))] ;to be corrected
      [(> arg1 arg2) (set! return #`(py-gt #,@(python-parser #'arg1) #,@(python-parser #'arg2)))] ;to be corrected
      [(>= arg1 arg2) (set! return #`(py-ge #,@(python-parser #'arg1) #,@(python-parser #'arg2)))] ;to be corrected
      #;[(py-add expr1 expr2) (set! return #`(#,@(python-parser #'expr1) #'+ (python-parser #'expr1)))] ;to be corrected
      ;[arg #'arg]
      [_ (displayln "end-python")]))
  
  (define (processing-parser aux)
    (displayln "Write-back-Processing")
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt < <= p-not p-lt p-le p-gt p-ge p-add p-truth)
      [(not stuff) (set! return #`(p-not #,@(processing-parser #'stuff)))] ;to be corrected
      ;[(p-truth arg) #`(#,@(processing-parser #'arg)) ] ;;call with arg
      [(< arg1 arg2) (set! return (write-processing #'(p-lt arg1 arg2)))]
      [(<= arg1 arg2) (set! return (write-processing #'(p-le arg1 arg2)))]
      [(> arg1 arg2) (set! return #`(p-gt #,@(processing-parser #'arg1) #,@(processing-parser #'arg2)))] ;to be corrected
      [(>= arg1 arg2) (set! return #`(p-ge #,@(processing-parser #'arg1) #,@(processing-parser #'arg2)))] ;to be corrected
      #;[(p-add expr1 expr2) (set! return #`(#,@(processing-parser #'expr1) #'+ (processing-parser #'expr1)))]
      ;[arg #'arg]
      [_ (displayln "end-processing")]))
  (display "in write-back ")
  (displayln arg)
  (cond [python (python-parser arg)]
        [processing (processing-parser arg)])
  return)

(define (translator language arg)
  (define arg-aux (create-meta-lang arg))
  (set-all language) ;decides what language to output (might change to write-back-meta-lang)
  (write-back-meta-lang arg-aux))

(define (set-all arg)
  (case arg
    [(python)
     (set! python #t)
     (set! processing #f)
     (set! racket #f)]
    [(processing)
     (set! python #f)
     (set! processing #t)
     (set! racket #f)]
    [(racket)
     (set! python #f)
     (set! processing #f)
     (set! racket #t)]))
;;#<syntax:3:0 (define-py-function :fib with-params (n) (lambda (:n) (cond ((py-truth (py-eq :n 0)) 0) ((py-truth (py-eq :n 1)) 1) (else (py-add (py-call :fib (py-sub :n 1)) (py-call :fib (py-sub :n 2)))))))>
;;((#%module-begin (p-function (fib-I-fn n) (let/ec return (p-block (p-if (p-lt-eq n 0) (return 0)) (p-if (p-eq n 1) (return 1)) (return (p-add (p-call #:call fib-I-fn (p-sub n 1)) (p-call #:call fib-I-fn (p-sub n 2))))))) (p-initialize)))>)
;;Test python
#;(define arg #'(define-py-function :fib with-params (n) (lambda (:n) (cond ((py-truth (py-eq :n 0)) 0) ((py-truth (py-eq :n 1)) 1) (else (py-add (py-call :fib (py-sub :n 1)) (py-call :fib (py-sub :n 2))))))))
#;(create-meta-lang arg)

;;Test processing