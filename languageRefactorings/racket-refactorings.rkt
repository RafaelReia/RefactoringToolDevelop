#lang racket
(require syntax/parse)
(provide racket-parser)
;;;;List of Racket Refactoring Rules
;;missing organiza import, add-prefix, extract function, rename, etc
(define (write-back sexp)
  (displayln "racket-refactorings Test"))

(define racket-parser
  (syntax-parser
    #:datum-literals (cons if not > <= >= < and lambda map length list)
    [(not (> a b))
     (write-back #'(<= a b))]
    [(not (<= a b))
     (write-back #'(> a b))]
    [(not (< a b))
     (write-back #'(>= a b))]
    [(not (>= a b))
     (write-back #'(< a b))]
    [(if test-expr then-expr else-expr)
     (begin
       (when (and (not (syntax->datum #'then-expr)) (syntax->datum #'else-expr))
         (write-back #'(not test-expr)))
       (when (and (syntax->datum #'then-expr) (not (syntax->datum #'else-expr)))
         (write-back #'test-expr)))]
    [(and (< x y) (< v z))
     (when (equal? (syntax->datum #'y) (syntax->datum #'v))
       (write-back #'(< x y z)))]
    [(and (> x y) (> v z))
     (when (equal? (syntax->datum #'y) (syntax->datum #'v))
       (write-back #'(> x y z)))]
    [(cons x (list y v ...))
     (write-back #'(list x y v ...))]
    [(= (length l) 0) (write-back #'(null? l))]
    ;[(= (length l) 1) (write-back #'(singleton? l))] this does not exist?
    ;[(cons x (list y ... v)) (write-back #'(list x y ... v))]
    [(ft (lambda (arg-aux) (ftn arg-aux2)) arg)  #:when (eq? (syntax-e #'arg-aux) (syntax-e #'arg-aux2)) (write-back #'(ft ftn arg))]
    [((lambda (arg-aux) (function arg-aux2)) arg)  #:when (eq? (syntax-e #'arg-aux) (syntax-e #'arg-aux2)) (write-back #'(function art))]))


(define (cond-to-if parent text start-selection end-selection start-line end-line binding-aux)
  (define arg null)
  (define (write-if conds thens last-else)
    (define aux-result null)
    (define result null)
    (define (write-aux conds thens)
      (unless (null? conds)
        (begin
          ;(displayln (syntax? conds))
          (set! aux-result (cons #`(#,(car conds) #,(car thens)) aux-result))
          (write-aux (cdr conds) (cdr thens))))) 
    (define (write-to-if aux-result last-else)
      (define (create-if conds)
        (if (null? conds)
            last-else
            #`(if (#,@(car (syntax-e (car conds))))
                  #,@(cdr (syntax-e (car conds)))
                  #,(create-if (cdr conds)))))
      (create-if aux-result))
    (write-aux (syntax-e conds) (syntax-e thens))
    (set! result #`(#,@(write-to-if (reverse aux-result) last-else)))
    #;(displayln (syntax->datum result))
    result)
  
  (syntax-parse arg
    [(cond (~seq (e:expr then-stuff) ... [else stuff]))
     (begin 
       #'(e ...)
       (write-back (write-if #'(e ...) #'(then-stuff ...) #'stuff)))]))

(define (if-to-cond parent text start-selection end-selection start-line end-line binding-aux)
  (define arg null)
  (define (parser1 stx)
    (define stx-aux null)
    (define list-tests (list))
    (define list-thens (list))
    (define else-aux null)
    (define (create-conds list-tests)
      (define result null)
      
      (define (create-result lst)
        (unless (null? list-tests)
          (cond [(syntax? list-tests)  (displayln "syntax reached")
                                       (set! result (cons #`(#,list-tests #,list-thens) result))]
                [(pair? list-tests)  (displayln "pair reached")
                                     (set! result (cons #`[#,(car list-tests) #,(car list-thens)] result))
                                     (displayln result)
                                     (set! list-tests (cdr list-tests))
                                     (set! list-thens (cdr list-thens))
                                     (create-result list-tests)]
                [else (displayln "else reached")])))
      (displayln "Creating the result")
      (create-result list-tests)
      (displayln result)
      result) 
    (define (parse-if stx)
      (syntax-parse stx
        [(if test-expr then-expr else-expr)  
         (begin 
           (set! list-tests (cons #'test-expr list-tests))
           (set! list-thens (cons #'then-expr list-thens))
           (parse-if #'else-expr))]
        [else (begin
                (displayln "end")
                (displayln (reverse list-tests))
                (displayln (reverse list-thens))
                (displayln #'else)
                (set! else-aux #'else)
                )]))
    (parse-if stx)
    (write-back #`(cond 
                    #,@(create-conds list-tests)
                    [else #, else-aux])))
  (parser1 arg))


;;;;; named-let to define (create a function)
;;; (let proc-id ?x ?y)
(syntax-parse #'(let* teste ((a 1 ) (b 2)) (begin (+ a b) (+ b a)))
  #:literals(let*)
  [(let* name ((i e:expr) ...) ?y) #'(define (name i ...) ?y)])

;;;;
#;(syntax-parse #'(define (foo a b c) img (+ 1 2 3))
  #:literals (define)
  [(define (nome arg e e2 ...) img body ...) #'(define (nome arg ...) (lambda (point-at arg e e2) ... (let ((arg arg) ...) body)))])

;;;;; let* to defines (use with caution!!)


;;;;;;;;;;;; If to When
(syntax-parse #'(if (< a b) #t (void))
  #:literals (if)
  [(if test-expr then-expr else-expr) 
   (when (equal? '(void) (syntax->datum #'else-expr)) 
     (syntax->datum #'(when test-expr then-expr)))])

;;;;;;;;;;;; If to Unless
(syntax-parse #'(if (< a b) (void) #f)
  #:literals (if)
  [(if test-expr then-expr else-expr)
   (when (equal? '(void) (syntax->datum #'then-expr)) 
     (syntax->datum #'(unless test-expr else-expr)))])

;;;;;;;;; (if (cond) #t (cond)) to OR
(syntax-parse #'(if (< 1 2) #t (< 1 3))
  #:literals(if)
  [(if test-expr #t else-expr)
   (when (and (boolean? (eval-syntax #'test-expr)) (boolean? (eval-syntax #'else-expr)))
     (syntax->datum #'(or test-expr else-expr)))])



;;;;;;;;; (if (cond) (cond) #f) to AND
(syntax-parse #'(if (< 1 2) (< 1 3) #f)
  #:literals(if)
  [(if test-expr then-expr #f)
   (when (and (boolean? (eval-syntax #'test-expr)) (boolean? (eval-syntax #'then-expr)))
     (syntax->datum #'(and test-expr then-expr)))])

;;;;;;;; (and (and ?x ... ) ?y...) -> (and ?x ... ?y...)
(syntax-parse #'(and (and (< 1 2) (< 2 3)) (< 3 4) (< 4 5))
  #:literals(and)
  [(and (and ?x ...) ?y ...) #'(and ?x ... ?y ...)])

;;;;;;;; (if ?x ?y #f) -> (when ?x ?y)
(syntax-parse #'(if (< 1 2) (< 2 3) #f)
  #:literals(if)
  [(if ?x ?y #f) #'(when ?x ?y)])

;;;;;;; (when ?x (begin ?y ...)) -> (when ?x ?y ...)
(syntax-parse #'(when (< 1 2) (begin (< 3 2) (< 3 4)))
  #:literals(when begin)
  [(when ?x (begin ?y ...)) #'(when ?x (?y ...))])

;;;;; (let ?x (begin ?y...)) -> (let ?x ?y ...)

(syntax-parse #'(let ((a 1 ) (b 2)) (begin (+ a b) (+ b a)))
  #:literals(let begin)
  [(let ?x (begin ?y ...)) #'(let ?x ?y ...)])
  


;;;;;;;; (and (= stuff 1) (= another-stuff 1))
;;;TODO add ... eq? (and related)
(syntax-parse #'(and (= (+ 1 2) 1) (= (+ 2 1) 1))
  #:literals(and = eq?)
  [(and (= expr val) (= expr2 val2))
   (when (eq? (syntax->datum #'val) (syntax->datum #'val2))
     (syntax->datum #'(and expr expr2 val)))])
