#lang racket
(require drracket/check-syntax)

#;(define arrows-collector%
    (class (annotations-mixin object%)
      (super-new)
      (define/override (syncheck:find-source-object stx)
        stx)
      (define/override (syncheck:add-arrow/name-dup/pxpy
                        start-source-obj start-left start-right start-px start-py
                        end-source-obj end-left end-right end-px end-py
                        actual? phase-level require-arrow? name-dup?)
        (set! arrows
              (cons (list start-source-obj end-source-obj)
                    arrows)))
      (define arrows '())
      (define/public (get-collected-arrows) arrows)
      (displayln "here")))

#;(send (make-object arrows-collector%) get-collected-arrows)

(define arrows-collector%
  (class (annotations-mixin object%)
    (super-new)
    (define/override (syncheck:find-source-object stx)
      stx)
    (define/override (syncheck:add-arrow/name-dup/pxpy
                        start-source-obj start-left start-right start-px start-py
                        end-source-obj end-left end-right end-px end-py
                        actual? phase-level require-arrow? name-dup?)
        (set! arrows
              (cons (list start-source-obj end-source-obj)
                    arrows)))
    (define arrows '())
    (define/public (get-collected-arrows) arrows)))
(define collector (new arrows-collector%))
(define (arrows form)
  (define base-namespace (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal base-namespace #f))
  #;(define collector (new arrows-collector%))
  (parameterize ([current-annotations collector]
                 [current-namespace base-namespace])
    (add-syntax (expand form))
    (done))
  (send collector get-collected-arrows))
(define (make-id name pos orig?)
  (datum->syntax
   #f
   name
   (list #f #f #f pos (string-length (symbol->string name)))
   (and orig? #'is-orig)))
(arrows `(λ (,(make-id 'x 1 #t)) ,(make-id 'x 2 #t)))
#;(arrows `(λ (x) x))
(arrows `(λ (,(make-id 'c 1 #t)) ,(make-id 'c 2 #t)))
#;(arrows `(λ (,(make-id 'x 1 #t)) x))


