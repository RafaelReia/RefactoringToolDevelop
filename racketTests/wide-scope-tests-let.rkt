#lang racket
(define teste 1)
(+ teste 2)
(let ((teste 3))
  (+ teste 2))
(+ teste 2)
(let ((teste teste))
  (+ teste 2)) ;;this does not work
(+ teste 2)

(+ teste 2) ;;; after this line I get an error...