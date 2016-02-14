#lang racket
(require syntax/parse)
(provide racket-parser)
;;;;List of Racket Refactoring Rules
;;missing organiza import, add-prefix, extract function, rename, etc
(define (write-back sexp)
  (displayln "racket-refactorings Test"))

(define (racket-parser arg)
  (displayln "inside racket-parser")
  (displayln arg)
  (define return (void))
  ;;;;Before general rules to not overlap.
 
  
  (syntax-parse arg
    #:datum-literals (cons if not > <= >= < and lambda map length list when begin let)
    [(not (> a b))
     (set! return #'(<= a b))]
    [(not (<= a b))
     (set! return #'(> a b))]
    [(not (< a b))
     (set! return #'(>= a b))]
    [(not (>= a b))
     (set! return #'(< a b))]
    [(if test-expr #f #t) (set! return #'(not test-expr))]
    [(if test-expr #t #f) (set! return #'test-expr)]
    [(if test-expr #t else-expr)
     (set! return #'(or test-expr else-expr))]
    [(if ?x ?y #f) (set! return #'(when ?x ?y))] ;;;;;;;; (if ?x ?y #f) -> (when ?x ?y) ;;Should be improved, automatic refactoring might have a deadlock
    [(if test-expr then-expr #f)
     (set! return #'(and test-expr then-expr))
     #;(when (and (boolean? (eval-syntax #'test-expr)) (boolean? (eval-syntax #'then-expr)))
       (set! return #'(and test-expr then-expr)))]
    [(if test-expr then-expr else-expr)
     (begin
       #;(when (and (not (syntax->datum #'then-expr)) (syntax->datum #'else-expr))
         (set! return #'(not test-expr))) ;;; buggyy
       #;(when (and (syntax->datum #'then-expr) (not (syntax->datum #'else-expr)))
         (set! return #'test-expr)) ;;;buggy
       (when (equal? '(void) (syntax->datum #'else-expr)) 
         (set! return #'(when test-expr then-expr)))
       (when (equal? '(void) (syntax->datum #'then-expr)) 
         (set! return #'(unless test-expr else-expr))))]
    [(and (< x y) (< v z))
     (when (equal? (syntax->datum #'y) (syntax->datum #'v))
       (set! return #'(< x y z)))]
    [(and (> x y) (> v z))
     (when (equal? (syntax->datum #'y) (syntax->datum #'v))
       (set! return #'(> x y z)))]
    [(cons x (list y v ...))
     (set! return #'(list x y v ...))]
    [(= (length l) 0) (set! return #'(null? l))]
    ;[(= (length l) 1) (write-back #'(singleton? l))] this does not exist?
    ;[(cons x (list y ... v)) (write-back #'(list x y ... v))]
    ;[(and (and ?x ...) ?y ...) (set! return #'(and ?x ... ?y ...))] ;;;;;;;; (and (and ?x ... ) ?y...) -> (and ?x ... ?y...)
    [(and (and ?x ...) (and ?y ...)) (set! return #'(and ?x ... ?y ...))]
    [(and (and ?x ...) ?y ...) (set! return #'(and ?x ... ?y ...))]
    [(and ?y ... (and ?x ...) ) (set! return #'(and ?x ... ?y ...))]
    [(when ?x (begin ?y ...)) (set! return #'(when ?x (?y ...)))] ;;;;;;; (when ?x (begin ?y ...)) -> (when ?x ?y ...)
    [(let ?x (begin ?y ...)) (set! return #'(let ?x ?y ...))] ;;;;; (let ?x (begin ?y...)) -> (let ?x ?y ...)
    [(ft (lambda (arg-aux) (ftn arg-aux2)) arg)  #:when (eq? (syntax-e #'arg-aux) (syntax-e #'arg-aux2)) (set! return #'(ft ftn arg))] ;this has a bug.
    [((lambda (arg-aux) (function arg-aux2)) arg)  #:when (eq? (syntax-e #'arg-aux) (syntax-e #'arg-aux2))  (set! return #'(function art))]
    [(let* name ((i e:expr) ...) ?y) (set! return #'(define (name i ...) ?y))] ;;; Add call to the defined because otherwise it is not correct
    [_ (void)])
  
  #;(define (cond-to-if arg) ;;Improve
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
      (set! return #`(#,@(write-to-if (reverse aux-result) last-else))))
    
    (syntax-parse arg
      #:datum-literals(cond)
      [(cond (~seq (e:expr then-stuff) ... [else stuff]))
       (begin 
         #'(e ...)
         (write-back (write-if #'(e ...) #'(then-stuff ...) #'stuff)))]
      [_ (void)]))
  (define (if-to-cond arg)
    (define result null)
    (define (parser1 stx)
      (define stx-aux null)
      (define list-tests (list))
      (define list-thens (list))
      (define else-aux null)
      
      (define (create-conds list-tests)
        (define result null)
        
        (define (create-result lst)
          (unless (null? list-tests)
            (cond [(syntax? list-tests) (set! result (cons #`(#,list-tests #,list-thens) result))]
                  [(pair? list-tests)  (set! result (cons #`[#,(car list-tests) #,(car list-thens)] result))
                                       (set! list-tests (cdr list-tests))
                                       (set! list-thens (cdr list-thens))
                                       (create-result list-tests)]
                  [else (displayln "else reached")])))
        (create-result list-tests)
        (displayln result)
        result) 
      (define (parse-if stx)  ;;; This has a bug!! improve
        (syntax-parse stx
          [(if test-expr then-expr else-expr)  
           (begin
             ;;add check in here, check weather the first else-expr has an if. (tricky hack :( )
             (set! list-tests (cons #'test-expr list-tests))
             (set! list-thens (cons #'then-expr list-thens))
             (parse-if #'else-expr))]
          [else (begin
                  (displayln (reverse list-tests))
                  (displayln (reverse list-thens))
                  (displayln #'else)
                  (set! else-aux #'else)
                  )]))
      (parse-if stx)
      (unless (null? list-tests)
        (set! result #`(cond 
                         #,@(create-conds list-tests)
                         [else #, else-aux]))))
    (parser1 arg)

    (unless (null? result)
      (set! return result)))
  #;(when (void? return)
  #;(cond-to-if arg) ;;bugs, a lot of false positives
  (if-to-cond arg)) ;;has a slighty bug that needs to be corrected.
  return)






#|
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
|#