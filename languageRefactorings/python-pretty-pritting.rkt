#lang racket
(provide write-python)
(require syntax/parse)
(define (write-python aux)
  
  (define (parse-python-to-racket aux)
    (syntax-parse aux
      #:datum-literals (:False :True expr-stmt not)
      [(not stuff) #`(not #,@(parse-python-to-racket #'stuff)) ]
      [(py-truth arg) #`(#,@(parse-python-to-racket #'arg)) ] ;;call with arg
      [(py-lt arg1 arg2) (displayln "teste") #'(arg1 < arg2)]
      [_ (displayln "fail")]))
  (displayln "teste")
  (parse-python-to-racket aux))

;;;;
;; if -> if
;; (py-truth (stuff)) -> (stuff)
;; (py-lt args ...)  -> ((car arg) < (cdr arg))
;; 

(write-python #'(not (py-truth (py-lt 1 2))))



;;
;;;