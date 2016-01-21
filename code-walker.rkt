#lang racket/base
(provide code-walker
         code-walker-non-expanded);805 linhas inicio

(define (code-walker code start end lastline) ;;for the expanded version
  (define start-line start)
  (define end-line end)
  (define aux (get-syntax-aux code start-line end-line lastline))
  (unless (null? aux)
    (parameterize ((print-syntax-width 9000))
      (displayln (cdr (syntax-e aux)))) ;;clean #%app
    (syntax-e aux))
    (displayln "result")
  (displayln aux)
  aux)

(define (code-walker-non-expanded code start end lastline)
  (define start-line start)
  (define end-line end)
  (displayln (syntax-e code))
  (displayln " NON EXPANDED")
  (define result (get-syntax-aux code start-line end-line lastline))
  (displayln "result")
  (displayln result)
  result)

(define (get-syntax-aux program start end lastline)
  (define stop? #f)
  (define source-stack (list))
  (define aux-result null)
  #;(displayln (syntax-source program))
  (define source-location (syntax-source program))
  (define (get-syntax program start end)
    (define source-aux program)
    (define check-line #t)
    (define result null)
    (define aux-result? #t)
    #;(displayln "source-aux is syntax? ")
    #;(displayln (syntax? source-aux))
    #;(parameterize ((print-syntax-width 9000))
        (displayln source-aux)
        (displayln source-stack))
    (define (get-next-compare source-aux source-stack)
      ;else says its bigger than the last part of the selection, could be the end of the program either. this happens when there is no next element.
      #;(displayln "SOURCE-AUX")
      #;(displayln source-aux)
      #;(displayln "NEXT COMPARE")
      (define aux (+ end 1))
      #;(parameterize ((print-syntax-width 9000))
        (displayln source-stack)
        (displayln (syntax? source-stack))
        #;(unless (null? source-stack)
            (displayln (car source-stack))))
      #;(displayln "is it true?")
      #;(displayln (and (pair? source-stack) (pair? (car source-stack)) (syntax? (car (car source-stack)))))
      (if (and (pair? source-stack) (pair? (car source-stack)) (syntax? (car (car source-stack))))
          (set! aux (syntax-line (car (car source-stack))))
          (when (or (null? source-stack) (and (syntax? (car source-stack)) (null? (syntax-e (car source-stack)))))
            (set! aux -1)))
      aux)
    (cond [(null? source-aux)
           (set! stop? #t)
           #;(displayln "It's null")]
          [stop? (displayln "evaluation stopped")]
          [(pair? source-aux)
           #;(displayln "It's pair")
           (set! source-stack (cons (cdr source-aux) source-stack)) ;;add to stack
           (set! source-aux (car source-aux))
           (get-syntax source-aux start end)]
          [(syntax? source-aux)
           (define compare-aux (syntax-line source-aux))
           (define next-compare null)
           (if (and (null? source-stack) (syntax? source-aux))
               (set! source-aux (syntax-e source-aux))
               (begin
                 (set! next-compare (get-next-compare source-aux source-stack))
                 #;(displayln start)
                 #;(displayln compare-aux)
                 #;(displayln next-compare)
                 (cond [(and (real? next-compare) (< next-compare 0))
                        ;go deeper
                        (set! source-aux (syntax-e source-aux))] 
                       [(not (real? compare-aux)) 
                        ;next one
                        (set! source-aux (car source-stack))
                        (set! source-stack (cdr source-stack))]
                       [(and (real? next-compare) (>= start next-compare))
                        ;next one
                        (set! source-aux (car source-stack))
                        (set! source-stack (cdr source-stack))]
                       [(> start compare-aux) ;; in the middle, enter
                        (set! source-aux (syntax-e source-aux))]
                       [(and (syntax-source source-aux)
                             (not (and (symbol? source-location) (syntax-source source-aux)))
                             (not (string=? (path->string (syntax-source source-aux))
                                                                       (path->string source-location))))
                        ;next one
                        #;(displayln "############ Skip ############")
                        (set! source-aux (car source-stack))
                        (set! source-stack (cdr source-stack))]
                       
                       [(> compare-aux lastline) ;(> next-compare lastline)
                        ;next one
                        #;(displayln "############ Skip ############")
                        (set! source-aux (car source-stack))
                        (set! source-stack (cdr source-stack))]
                       [(<= start compare-aux end) ;; starts in the selected place, and it is not bigger then the next one.
                        ;(set! source-aux (syntax-e source-aux))
                        #;(display "FOUND IT! ")
                        #;(displayln source-aux)
                        (set! stop? #t)
                        (set! aux-result source-aux)]
                       [(and (>= compare-aux start) (<= compare-aux lastline)) ;;needed for Python implementation
                        #;(displayln "############ Skip ############")
                        (set! source-aux (car source-stack))
                        (set! source-stack (cdr source-stack))]
                       [else
                        (displayln "This should not happen")
                        (read)
                        (displayln "weird else")])))
           #;(parameterize ((print-syntax-width 9000))
               (displayln "In Syntax before leaving")
               (displayln source-aux)
               (displayln source-stack))
           (get-syntax source-aux start end)]
          [else
           (set! source-aux (car source-stack))
           (set! source-stack (cdr source-stack))
           (get-syntax source-aux start end)]))
  (get-syntax program start end)
  aux-result)