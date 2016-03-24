#lang racket

#;(find-files (lambda (a)(string-suffix? (path->string a) ".rkt")) #f)

#;(open-input-file "share/racket/collects/RefactoringTool/racketTests/foo.rkt")

(define aux (find-files (lambda (a)(string-suffix? (path->string a) ".in")) #f))
(list? aux)
(displayln aux)
(string-trim (format "~a" (car aux)) ".in")
(displayln (car aux))
(file->string (string-append (string-trim (format "~a" (car aux)) ".in") ".out"))
(displayln (find-files (lambda (a)(string-suffix? (path->string a) ".out")) #f))
(open-input-file (car aux))


;#<syntax:1:0 (module anonymous-module racket (#%module-begin (not (> 1 2))))>
(file->string "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/testnot.out")
(if (file-exists? "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/testnot5.out")
    (file->string "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/testnot5.out")
    (displayln "not"))

#;(make-file-or-directory-link "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/teste" "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/teste")


#;(rename-file-or-directory
(format "~a" (make-temporary-file "rkttmp~a" #f "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/"))
"/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/try.out")

#;(define out (open-output-file "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/try.out"))

(define out (open-output-file "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/rafael.out"))
(write "Hello world" out)


(close-output-port out)