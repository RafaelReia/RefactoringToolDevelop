#lang racket
(require syntax/parse)
(require syntax/to-string)
(require "processing-pretty-pritting.rkt")
(provide processing-parser)
;;;;List of Processing Refactoring Rules


(define (processing-parser arg)
   (displayln "inside processing-parser")
  (displayln arg)
  (define return (void))
  (syntax-parse arg
     #:datum-literals (p-not p-declaration p-and p-or p-lt p-gt p-le p-ge)
    [(p-declaration (name stuff)) (set! return #`(#,@(processing-parser #'stuff)))]
    [(p-not (p-gt a b))
     (set! return (write-processing #'(p-le a b)))]
    [(p-not (p-le a b))
     (set! return (write-processing #'(p-gt a b)))]
    [(p-not (p-lt a b))
     (set! return (write-processing #'(p-ge a b)))]
    [(p-not (p-ge a b))
     (set! return (write-processing #'(p-lt a b)))]
    ;;(if (py-truth (py-lt 1 2)) :True :False) to (py-truth (py-lt 1 2)) to (1 < 2)
    #;[(if (py-turth (py-lt arg arg2)) :True :False)
     (displayln "fail 2")]
    ;;;;(list py-turth cond)  !!!!
    [_ (begin (displayln "none in processing :(" )(void))])
  (displayln "debug processing")
  (displayln return)
  return)


#;(define (processing-parser1 arg)
  (syntax-parse arg
     #:datum-literals (p-not p-and p-or p-lt p-gt p-le p-ge)
    [(p-not (p-gt a b)) #'(p-le a b)]
    [(p-not (p-le a b)) #'(p-gt a b)]
    [(p-not (p-lt a b)) #'(p-ge a b)]
    [(p-not (p-ge a b)) #'(p-lt a b)]
    [_ (void)]))