#lang racket
(provide xpto)

(define (abc)
  1)

(define-syntax-rule (xpto)
  (abc))