#lang racket
(require drracket/check-syntax)

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
    (define/public (get-collected-arrows) arrows)
    
    (define/private (process-trace-element known-dead-place-channels defs-text x)
      ;; using 'defs-text' all the time is wrong in the case of embedded editors,
      ;; but they already don't work and we've arranged for them to not appear here ....
      (match x
        [`#(syncheck:add-arrow/name-dup/pxpy
            ,start-pos-left ,start-pos-right ,start-px ,start-py
            ,end-pos-left ,end-pos-right ,end-px ,end-py
            ,actual? ,level ,require-arrow? ,name-dup-pc ,name-dup-id)
         (define name-dup? (build-name-dup? name-dup-pc name-dup-id  known-dead-place-channels))
         (send defs-text syncheck:add-arrow/name-dup/pxpy
               defs-text start-pos-left start-pos-right start-px start-py
               defs-text end-pos-left end-pos-right end-px end-py
               actual? level require-arrow? name-dup?)]
        [`#(syncheck:add-tail-arrow ,from-pos ,to-pos)
         (send defs-text syncheck:add-tail-arrow defs-text from-pos defs-text to-pos)]
        [`#(syncheck:add-mouse-over-status ,pos-left ,pos-right ,str)
         (send defs-text syncheck:add-mouse-over-status defs-text pos-left pos-right str)]
        [`#(syncheck:add-background-color ,color ,start ,fin)
         (send defs-text syncheck:add-background-color defs-text color start fin)]
        [`#(syncheck:add-jump-to-definition ,start ,end ,id ,filename ,submods)
         (send defs-text syncheck:add-jump-to-definition defs-text start end id filename submods)]
        [`#(syncheck:add-require-open-menu ,start-pos ,end-pos ,file)
         (send defs-text syncheck:add-require-open-menu defs-text start-pos end-pos file)]
        [`#(syncheck:add-docs-menu ,start-pos ,end-pos ,key ,the-label ,path ,definition-tag ,tag)
         (send defs-text syncheck:add-docs-menu defs-text start-pos end-pos
               key the-label path definition-tag tag)]
        [`#(syncheck:add-definition-target ,start-pos ,end-pos ,id ,mods)
         (send defs-text syncheck:add-definition-target defs-text start-pos end-pos id mods)]
        [`#(syncheck:add-id-set ,to-be-renamed/poss ,name-dup-pc ,name-dup-id)
         (define to-be-renamed/poss/fixed
           (for/list ([lst (in-list to-be-renamed/poss)])
             (list defs-text (list-ref lst 0) (list-ref lst 1))))
         (define name-dup? (build-name-dup? name-dup-pc name-dup-id known-dead-place-channels))
         (send defs-text syncheck:add-id-set to-be-renamed/poss/fixed name-dup?)]))
    
    (define/private (build-name-dup? name-dup-pc name-dup-id known-dead-place-channels)
      (define (name-dup? name) 
        (cond
          [(hash-ref known-dead-place-channels name-dup-pc #f)
           ;; just give up here ...
           #f]
          [else
           (place-channel-put name-dup-pc (list name-dup-id name))
           (define res (sync/timeout .5 (handle-evt name-dup-pc list)))
           (cond
             [(list? res) (car res)]
             [else
              (hash-set! known-dead-place-channels name-dup-pc #t)
              #f])]))
      name-dup?)))
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
