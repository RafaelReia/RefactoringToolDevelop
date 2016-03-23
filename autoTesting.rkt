#lang racket

#;(find-files (lambda (a)(string-suffix? (path->string a) ".rkt")) #f)

#;(open-input-file "share/racket/collects/RefactoringTool/racketTests/foo.rkt")

(define aux (find-files (lambda (a)(string-suffix? (path->string a) ".in")) #f))
(list? aux)
(displayln aux)
(displayln (find-files (lambda (a)(string-suffix? (path->string a) ".out")) #f))
(open-input-file (car aux))


;#<syntax:1:0 (module anonymous-module racket (#%module-begin (not (> 1 2))))>
(file->string "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/testnot.out")