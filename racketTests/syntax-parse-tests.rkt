#lang racket/base
(cond [(< 1 2) 1]
       [(< 2 3) 2]
       [(< 4 5) 3]
       [else 4])

(not (> 1 2))

(define a 1)
(define b 2)
(define c 3)



(if (< a b)
    1
    (if (< b a )
        2
        (if (< a c)
            5
            (if (< c a)
                3
                4))))



(if (= (+ a b) a) 
    #f 
    #t)
;;;Not
(not (> a b))
(not (>= a b))
(not (< a b))
(not (<= a b))
;; IF
(if (= (+ a b) a) 
    #f 
    #t)
(if (= (+ a b) a)
    #t
    #f)
(define (foo n)
  (displayln "ola")
  n)
;;; And
(and (< (foo 1) (foo 2)) (< (foo 2) 3))
(and (> 3 2) (> 2 1))

(define l (list))

(= (length l) 0)
;(= (length l) 1)

(cons 1 (list 2 3 4 5 6))


(map (lambda (l) (car l)) (list (list 1 2) (list 3 4)))
(map (lambda (l) (car l)) '((1 2) (3 4)))
;(map car '((1 2)(3 4)))

;;future work. explore phases + scopes



(displayln "end")

(not (<= a b))








#|(provide print-cake)
(provide a-function)

(define (a-function)
  (void))

; draws a cake with n candles
(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))
 
(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))|#