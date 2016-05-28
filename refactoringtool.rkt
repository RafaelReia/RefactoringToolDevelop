#lang racket/base
(require drracket/tool ;Bug, racket do not find the "binding" (paint that red) however it is needed)
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         framework
         syntax/parse
         racket/pretty
         drracket/private/syncheck/syncheck-intf
         syntax/toplevel
         drracket/private/syncheck/traversals
         (for-template racket/base) ;was a test, did not wohirk
         syntax/to-string
         data/interval-map
         racket/set
         racket/dict
         racket/runtime-path
         racket/match
         racket/place
         racket/list
         racket/string
         racket/file
         string-constants
         framework
         (prefix-in drracket:arrow: drracket/arrow)
         (prefix-in fw: framework/framework)
         ;racket/string
         "online-comp.rkt"
         "languageRefactorings/racket-refactorings.rkt"
         "languageRefactorings/python-refactorings.rkt"
         "languageRefactorings/processing-refactorings.rkt"
         "languageRefactorings/meta-language.rkt"
         "languageRefactorings/python-pretty-pritting.rkt"
         "languageRefactorings/processing-pretty-pritting.rkt"
         "code-walker.rkt") 
(provide tool@)
(define expanded-program null)
(define non-expanded-program null)
(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define refactoring-tool-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text
                 get-current-tab
                 get-interactions-text
                 get-editor%
                 ;dc-location-to-editor-location ;;added check this
                 ;last-position ;;;added check this
                 )
        
        (define (dc-location-to-editor-location arg1 arg2)
          (send (get-editor%) dc-location-to-editor-location arg1 arg2))
        (inherit register-toolbar-button)
        (define (set-syncheck-running-mode mode)
          (cond
            [(not mode)
             (when (box? current-syncheck-running-mode)
               (set-box! current-syncheck-running-mode #f))
             (set! current-syncheck-running-mode #f)
             #t]
            [(box? mode)
             (cond
               [(eq? current-syncheck-running-mode 'button)
                #f]
               [(eq? mode current-syncheck-running-mode)
                ;; this shouldn't happen, I think
                #t]
               [else
                (when (box? current-syncheck-running-mode)
                  (set-box! current-syncheck-running-mode #f))
                (set! current-syncheck-running-mode mode)
                #t])]
            [(eq? 'button mode)
             (when (box? current-syncheck-running-mode)
               (set-box! current-syncheck-running-mode #f))
             (set! current-syncheck-running-mode mode)
             #t]
            [else
             (error 'set-syncheck-running-mode "unknown new mode ~s\n" mode)]))
        ;;;;;;;;;;;;;;;;;;;;;;;;;; Arrow Structure  ;;;;;
        (define-struct arrow () #:mutable #:transparent)
        (define-struct (var-arrow arrow)
          (start-text start-pos-left start-pos-right start-px start-py
                      end-text end-pos-left end-pos-right end-px end-py
                      actual? level require-arrow? name-dup?)
          ;; level is one of 'lexical, 'top-level, 'import
          #:transparent)
        (define-struct (tail-arrow arrow) (from-text from-pos to-text to-pos) #:transparent)
        
        ;; arrow-records : (U #f hash[text% => arrow-record])
        ;; arrow-record = interval-map[(listof arrow-entry)]
        ;; arrow-entry is one of
        ;;   - (cons (U #f sym) (menu -> void))
        ;;   - def-link
        ;;   - tail-link
        ;;   - arrow
        ;;   - string
        ;;   - colored-region
        (define/private (get-arrow-record text)
          (unless (object? text)
            (error 'get-arrow-record "expected a text as the second argument, got ~e" text))
          (hash-ref! arrow-records text (lambda () (make-interval-map))))
        
        (define arrow-records #f)
        
        (define/private (fetch-arrow-records txt pos)
          (and arrow-records
               (let ([im (hash-ref arrow-records txt #f)]) 
                 (if im
                     (interval-map-ref im pos '())
                     '()))))
        
        (define/public (dump-arrow-records)
          (displayln "inside dump-arrow-records")
          (cond
            [arrow-records
             (for ([(k v) (in-hash arrow-records)])
               (printf "\n\n~s:\n" k)
               (let loop ([it (interval-map-iterate-first v)])
                 (when it
                   (printf "~s =>\n" (interval-map-iterate-key v it))
                   (for ([v (in-list (interval-map-iterate-value v it))])
                     (printf "  ~s\n" v))
                   (printf "\n")
                   (loop (interval-map-iterate-next v it)))))]
            [else
             (printf "arrow-records empty\n")]))
        
        ;; cleanup-texts : (or/c #f (listof text))
        (define cleanup-texts #f)
        
        ;; definition-targets : hash-table[(list symbol[id-name] (listof symbol[submodname])) 
        ;;                                 -o> (list text number number)]
        (define definition-targets (make-hash))
        
        
        ;; bindings-table : hash-table[(list text number number)
        ;;                             -o> (setof (list text number number))]
        ;; this is a private field
        (define bindings-table (make-hash))
        
        ;; add-to-bindings-table : text number number text number number -> boolean
        ;; results indicates if the binding was added to the table. It is added, unless
        ;;  1) it is already there, or
        ;;  2) it is a link to itself
        (define/private (add-to-bindings-table start-text start-left start-right
                                               end-text end-left end-right)
          (cond
            [(and (object=? start-text end-text)
                  (= start-left end-left)
                  (= start-right end-right))
             #f]
            [else
             (define key (list start-text start-left start-right))
             (define priors (hash-ref bindings-table key (λ () (set))))
             (define new (list end-text end-left end-right))
             (cond
               [(set-member? priors new)
                #f]
               [else
                (hash-set! bindings-table key (set-add priors new))
                #t])]))
        ;; compare-bindings : (list text number number) (list text number number) -> boolean
        ;; compares two bindings in the sets inside the bindings table, returning
        ;; #t if l1 appears earlier in the file than l2 does.
        (define/private (syncheck:compare-bindings l1 l2)
          
          ;; find-dc-location : text number -> (values number number)
          (define (find-dc-location text pos)
            (send text position-location pos xlb xrb)
            (send text editor-location-to-dc-location (unbox xlb) (unbox xrb)))
          
          (let ([start-text (list-ref l1 0)]
                [start-left (list-ref l1 1)]
                [end-text (list-ref l2 0)]
                [end-left (list-ref l2 1)])
            (cond
              [(object=? start-text end-text)
               (< start-left end-left)]
              [else
               (let-values ([(sx sy) (find-dc-location start-text start-left)]
                            [(ex ey) (find-dc-location end-text end-left)])
                 (cond
                   [(= sy ey) (< sx ex)]
                   [else (< sy ey)]))])))
        
        (define tacked-hash-table (make-hasheq))
        
        ;; find-char-box : text number number -> (values number number number number)
        ;; returns the bounding box (left, top, right, bottom) for the text range.
        ;; only works right if the text is on a single line.
        (define/private (find-char-box text left-pos right-pos)
          (send text position-location left-pos xlb ylb #t)
          (send text position-location right-pos xrb yrb #f)
          (define-values (xl-off yl-off) 
            (send text editor-location-to-dc-location (unbox xlb) (unbox ylb)))
          (define-values (xl yl)
            (dc-location-to-editor-location xl-off yl-off))
          (define-values (xr-off yr-off)
            (send text editor-location-to-dc-location (unbox xrb) (unbox yrb)))
          (define-values (xr yr) (dc-location-to-editor-location xr-off yr-off))
          (values 
           xl
           yl
           xr 
           yr))
        
        (define/private (get-arrow-poss arrow)
          (cond
            [(var-arrow? arrow) (get-var-arrow-poss arrow)]
            [(tail-arrow? arrow) (get-tail-arrow-poss arrow)]))
        
        (define/private (get-var-arrow-poss arrow)
          (let-values ([(start-x start-y) (find-poss 
                                           (var-arrow-start-text arrow)
                                           (var-arrow-start-pos-left arrow)
                                           (var-arrow-start-pos-right arrow)
                                           (var-arrow-start-px arrow)
                                           (var-arrow-start-py arrow))]
                       [(end-x end-y) (find-poss 
                                       (var-arrow-end-text arrow)
                                       (var-arrow-end-pos-left arrow)
                                       (var-arrow-end-pos-right arrow)
                                       (var-arrow-end-px arrow)
                                       (var-arrow-end-py arrow))])
            (values start-x start-y end-x end-y)))
        
        (define/private (get-tail-arrow-poss arrow)
          ;; If the item is an embedded editor snip, redirect
          ;; the arrow to point at the left edge rather than the
          ;; midpoint.
          (define (find-poss/embedded text pos)
            (let* ([snip (send text find-snip pos 'after)])
              (cond
                [(and snip 
                      (is-a? snip editor-snip%)
                      (= pos (send text get-snip-position snip)))
                 (find-poss text pos pos .5 .5)]
                [else
                 (find-poss text pos (+ pos 1) .5 .5)])))
          (let-values ([(start-x start-y) (find-poss/embedded 
                                           (tail-arrow-from-text arrow)
                                           (tail-arrow-from-pos arrow))]
                       [(end-x end-y) (find-poss/embedded
                                       (tail-arrow-to-text arrow)
                                       (tail-arrow-to-pos arrow))])
            (values start-x start-y end-x end-y)))
        
        (define xlb (box 0))
        (define ylb (box 0))
        (define xrb (box 0))
        (define yrb (box 0))
        
        (define/private (find-poss text left-pos right-pos px py)
          (send text position-location left-pos xlb ylb #t)
          (send text position-location right-pos xrb yrb #f)
          (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location 
                                               (unbox xlb) (unbox ylb))]
                        [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                        [(xr-off yr-off) (send text editor-location-to-dc-location 
                                               (unbox xrb) (unbox yrb))]
                        [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
            (values (+ xl (* (- xr xl) px))
                    (+ yl (* (- yr yl) py)))))
        
        ;; syncheck:init-arrows : -> void
        (define/public (syncheck:init-arrows)
          (set! tacked-hash-table (make-hasheq))
          (set! arrow-records (make-hasheq))
          (set! bindings-table (make-hash))
          (set! cleanup-texts '())
          (set! definition-targets (make-hash)))
        
        #;(define/public (syncheck:arrows-visible?)
            (or arrow-records cursor-pos cursor-text cursor-eles cursor-tooltip))
        
        ;; syncheck:clear-arrows : -> void
        #;(define/public (syncheck:clear-arrows)
            (when (syncheck:arrows-visible?)
              (set! tacked-hash-table #f)
              (set! arrow-records #f)
              (when (update-latent-arrows #f #f)
                (update-drawn-arrows))
              (syncheck:clear-coloring)
              (invalidate-bitmap-cache/padding)))
        
        (define/public (syncheck:clear-coloring)
          (when cleanup-texts
            (for-each (λ (text) (send text thaw-colorer))
                      cleanup-texts))
          (set! cleanup-texts #f))
        
        
        (define/public (syncheck:add-background-color text start fin raw-color)
          (void)
          #;(displayln "add-backgroud-color not implemented")
          #;(read)
          #;(when arrow-records
              (when (is-a? text text:basic<%>)
                ;; we adjust the colors over here based on the white-on-black
                ;; preference so we don't have to have the preference set up
                ;; in the other place when running check syntax in online mode.
                (define color 
                  (if (preferences:get 'framework:white-on-black?)
                      (cond
                        [(equal? raw-color "palegreen") "darkgreen"]
                        [else raw-color])
                      raw-color))
                (add-to-range/key text start fin
                                  (make-colored-region color text start fin)
                                  #f #f))))
        
        ;; add-to-range/key : text number number any any boolean -> void
        ;; adds `key' to the range `start' - `end' in the editor
        ;; If use-key? is #t, it adds `to-add' with the key, and does not
        ;; replace a value with that key already there.
        ;; If use-key? is #f, it adds `to-add' without a key.
        ;; pre: arrow-records is not #f
        (define/private (add-to-range/key text start pre-end to-add key use-key?)
          (define end (if (= start pre-end) (+ start 1) pre-end))
          (when (<= 0 start end (send text last-position))
            (define arrow-record (get-arrow-record text))
            ;; Dropped the check (< _ (vector-length arrow-vector))
            ;; which had the following comment:
            ;;    the last test in the above and is because some syntax objects
            ;;    appear to be from the original source, but can have bogus information.
            
            ;; interval-maps use half-open intervals which works out well for positions
            ;; in the editor, since the interval [0,3) covers the characters just after
            ;; positions 0, 1, and 2, but not the character at position 3 (positions are
            ;; between characters)
            (cond [use-key?
                   (interval-map-update*! arrow-record start end
                                          (lambda (old)
                                            (if (for/or ([x (in-list old)])
                                                  (and (pair? x) (car x) (eq? (car x) key)))
                                                old
                                                (cons (cons key to-add) old)))
                                          null)]
                  [else
                   (interval-map-cons*!
                    arrow-record start end to-add null)])))
        
        ;; pre: start-editor, end-editor are embedded in `this' (or are `this')
        (define/public (syncheck:add-arrow/name-dup/pxpy start-text
                                                         start-pos-left start-pos-right
                                                         start-px start-py
                                                         end-text
                                                         end-pos-left end-pos-right
                                                         end-px end-py
                                                         actual? level require-arrow? name-dup?)
          (when (and arrow-records
                     (preferences:get 'drracket:syncheck:show-arrows?))
            (when (add-to-bindings-table
                   start-text start-pos-left start-pos-right
                   end-text end-pos-left end-pos-right)
              (let ([arrow (make-var-arrow start-text start-pos-left start-pos-right
                                           start-px start-py
                                           end-text end-pos-left end-pos-right
                                           end-px end-py
                                           actual? level require-arrow? name-dup?)])
                (add-to-range/key start-text start-pos-left start-pos-right arrow #f #f)
                (add-to-range/key end-text end-pos-left end-pos-right arrow #f #f)))))
        
        ;; syncheck:add-tail-arrow : text number text number -> void
        (define/public (syncheck:add-tail-arrow from-text from-pos to-text to-pos)
          (when (and arrow-records
                     (preferences:get 'drracket:syncheck:show-arrows?))
            (let ([tail-arrow (make-tail-arrow to-text to-pos from-text from-pos)])
              (add-to-range/key from-text from-pos (+ from-pos 1) tail-arrow #f #f)
              (add-to-range/key to-text to-pos (+ to-pos 1) tail-arrow #f #f))))
        
        ;; syncheck:add-jump-to-definition : text start end id filename -> void
        (define/public (syncheck:add-jump-to-definition text start end id filename submods)
          (void)
          #;(displayln "add-jump-to-definition not implemented")
          #;(read)
          #;(when arrow-records
              (add-to-range/key text start end (make-def-link id filename submods) #f #f)))
        
        ;; syncheck:add-mouse-over-status : text pos-left pos-right string -> void
        (define/public (syncheck:add-mouse-over-status text pos-left pos-right str)
          (void)
          #;(displayln "add-mouse-over-status not implemented")
          #;(when arrow-records
              (add-to-range/key text pos-left pos-right 
                                (make-tooltip-info text pos-left pos-right str)
                                #f #f)))
        
        
        
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;; Arrow Information ;;;;;
        (define/public (replay-compile-comp-trace-aux defs-text val bx) ;;;it is not calling this function fix this
          #;(displayln "in replay-compile-comp-trace")
          #;(send (send defs-text get-tab) add-bkg-running-color
                  'syncheck "orchid" cs-syncheck-running)
          (define known-dead-place-channels (make-hasheq))
          (let loop ([val val]
                     [start-time (current-inexact-milliseconds)]
                     [i 0])
            (cond
              [(and (null? val) (pair? (unbox bx)))
               (define new-val (car (unbox bx)))
               (set-box! bx (cdr (unbox bx)))
               (loop new-val start-time i)]
              [(null? val)
               #;(send defs-text syncheck:update-blue-boxes (send (send defs-text get-tab) get-ints))
               #;(send defs-text syncheck:update-drawn-arrows)
               #;(send (send defs-text get-tab) remove-bkg-running-color 'syncheck)
               (set-syncheck-running-mode #f)]
              [(not (unbox bx))
               ;; if we've been asked to stop (because some new results are ready
               ;; and another trace is running).
               (void)]
              [(and (i . > . 0)  ;; check i just in case things are really strange
                    (20 . <= . (- (current-inexact-milliseconds) start-time)))
               (queue-callback
                (λ ()
                  (when (unbox bx)
                    (loop val (current-inexact-milliseconds) 0)))
                #f)]
              [else
               #;(displayln val)
               (process-trace-element known-dead-place-channels defs-text (car val))
               (loop (cdr val) start-time (+ i 1))])))
        (define/private (process-trace-element known-dead-place-channels defs-text x)
          #;(displayln "in process-trace-element")
          ;; using 'defs-text' all the time is wrong in the case of embedded editors,
          ;; but they already don't work and we've arranged for them to not appear here ....
          #;(parameterize ((print-syntax-width 9000))
              (displayln defs-text)
              (pretty-print defs-text)
              (read)
              )
          
          (match x
            [`#(syncheck:add-arrow/name-dup/pxpy
                ,start-pos-left ,start-pos-right ,start-px ,start-py
                ,end-pos-left ,end-pos-right ,end-px ,end-py
                ,actual? ,level ,require-arrow? ,name-dup-pc ,name-dup-id)
             (define name-dup? (build-name-dup? name-dup-pc name-dup-id  known-dead-place-channels))
             (syncheck:add-arrow/name-dup/pxpy
              defs-text start-pos-left start-pos-right start-px start-py
              defs-text end-pos-left end-pos-right end-px end-py
              actual? level require-arrow? name-dup?)]
            [`#(syncheck:add-tail-arrow ,from-pos ,to-pos)
             (syncheck:add-tail-arrow defs-text from-pos defs-text to-pos)]
            [`#(syncheck:add-mouse-over-status ,pos-left ,pos-right ,str)
             (syncheck:add-mouse-over-status defs-text pos-left pos-right str)]
            [`#(syncheck:add-background-color ,color ,start ,fin)
             (syncheck:add-background-color defs-text color start fin)]
            [`#(syncheck:add-jump-to-definition ,start ,end ,id ,filename ,submods)
             (syncheck:add-jump-to-definition defs-text start end id filename submods)]
            [`#(syncheck:add-require-open-menu ,start-pos ,end-pos ,file)
             (void)
             #;(displayln "require-open-menu")
             #;(syncheck:add-require-open-menu defs-text start-pos end-pos file)] ;;got an error, might be useful...
            [`#(syncheck:add-docs-menu ,start-pos ,end-pos ,key ,the-label ,path ,definition-tag ,tag)
             (void)
             #;(displayln "add-docs-menu")
             #;(syncheck:add-docs-menu defs-text start-pos end-pos
                                       key the-label path definition-tag tag)]
            [`#(syncheck:add-definition-target ,start-pos ,end-pos ,id ,mods)
             (void)
             #;(displayln "add-definition-target")
             #;(syncheck:add-definition-target defs-text start-pos end-pos id mods)]
            [`#(syncheck:add-id-set ,to-be-renamed/poss ,name-dup-pc ,name-dup-id)
             (void)
             #;(define to-be-renamed/poss/fixed
                 (for/list ([lst (in-list to-be-renamed/poss)])
                   (list defs-text (list-ref lst 0) (list-ref lst 1))))
             #;(define name-dup? (build-name-dup? name-dup-pc name-dup-id known-dead-place-channels))
             #;(syncheck:add-id-set to-be-renamed/poss/fixed name-dup?)]))
        
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
          name-dup?)
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define definitions-text (get-definitions-text))
        (define text (get-definitions-text))
        
        (define drs-eventspace (current-eventspace))
        (define the-tab (get-current-tab))
        (define-values (old-break-thread old-custodian) (send the-tab get-breakables))
        
        ;; set by the init-proc
        (define expanded-expression void)
        (define expansion-completed void)
        (define user-custodian #f)
        
        (define normal-termination? #f)
        (define show-error-report/tab
          (void))
        (define cleanup
          (λ () ; =drs=
            (send the-tab set-breakables old-break-thread old-custodian)
            (send the-tab enable-evaluation)
            (set-syncheck-running-mode #f) 
            #;(close-status-line 'drracket:check-syntax:status)
            
            ;; do this with some lag ... not great, but should be okay.
            #;(let ([err-port (send (send the-tab get-error-report-text) get-err-port)])
                (thread
                 (λ ()
                   (flush-output err-port)
                   (queue-callback
                    (λ ()
                      (unless (= 0 (send (send the-tab get-error-report-text) last-position))
                        (show-error-report/tab)))))))))
        (define kill-termination
          (λ ()
            (unless normal-termination?
              (parameterize ([current-eventspace drs-eventspace])
                (queue-callback
                 (λ ()
                   #;(send the-tab syncheck:clear-highlighting)
                   (cleanup)
                   (custodian-shutdown-all user-custodian)))))))
        (define error-display-semaphore (make-semaphore 0))
        (define uncaught-exception-raised
          (λ () ;; =user=
            (set! normal-termination? #t)
            (parameterize ([current-eventspace drs-eventspace])
              (queue-callback
               (λ () ;;  =drs=
                 (yield error-display-semaphore) ;; let error display go first
                 #;(send the-tab syncheck:clear-highlighting)
                 (cleanup)
                 (custodian-shutdown-all user-custodian))))))
        #;(define error-port (send (send the-tab get-error-report-text) get-err-port)) ;;cant find get-error-report-text in racket doc
        #;(define output-port (send (send the-tab get-error-report-text) get-out-port))
        ;; with-lock/edit-sequence : text (-> void) -> void
        ;; sets and restores some state of the definitions text
        ;; so that edits to the definitions text work out.
        (define (with-lock/edit-sequence definitions-text thnk)
          (let* ([locked? (send definitions-text is-locked?)])
            (send definitions-text begin-edit-sequence #t #f)
            (send definitions-text lock #f)
            (thnk)
            (send definitions-text end-edit-sequence)
            (send definitions-text lock locked?)))
        (define definitions-text-copy 
          (new (class text:basic%
                 ;; overriding get-port-name like this ensures
                 ;; that the resulting syntax objects are connected
                 ;; to the actual definitions-text, not this copy
                 (define/override (get-port-name)
                   (send definitions-text get-port-name))
                 (super-new))))
        (define init-proc
          (λ () ; =user=
            (send the-tab set-breakables (current-thread) (current-custodian))
            #;(set-directory definitions-text)  ;Is this important?
            (current-load-relative-directory #f)
            #;(current-error-port error-port)
            #;(current-output-port output-port)
            #;(error-display-handler 
               (λ (msg exn) ;; =user=
                 (parameterize ([current-eventspace drs-eventspace])
                   #;(queue-callback
                      (λ () ;; =drs=
                        
                        ;; this has to come first or else the positioning
                        ;; computations in the highlight-errors/exn method
                        ;; will be wrong by the size of the error report box
                        (show-error-report/tab)
                        
                        ;; a call like this one also happens in 
                        ;; drracket:debug:error-display-handler/stacktrace
                        ;; but that call won't happen here, because
                        ;; the rep is not in the current-rep parameter
                        (send interactions-text highlight-errors/exn exn))))
                 
                 (drracket:debug:error-display-handler/stacktrace 
                  msg 
                  exn 
                  '()
                  #:definitions-text definitions-text)
                 
                 (semaphore-post error-display-semaphore)))
            
            (error-print-source-location #f) ; need to build code to render error first
            (uncaught-exception-handler
             (let ([oh (uncaught-exception-handler)])
               (λ (exn)
                 (uncaught-exception-raised)
                 (oh exn))))
            #;(update-status-line 'drracket:check-syntax:status status-expanding-expression)
            (set!-values (expanded-expression expansion-completed) 
                         (make-traversal (current-namespace)
                                         (current-directory)
                                         #f)) ;; set by set-directory above #:print-extra-info? [print-extra-info? #f]
            (set! user-custodian (current-custodian))))
        (define settings (send definitions-text get-next-settings))
        (define module-language?
          (is-a? (drracket:language-configuration:language-settings-language settings)
                 drracket:module-language:module-language<%>))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (define current-syncheck-running-mode #f)
        
        ;;;;;aux functions to rename menu callback
        
        ;; position->matching-identifiers-hash 
        ;; : txt pos pos -> (values (listof var-arrow?) hash[(list txt pos pos) -o> #t])
        (define/private (position->matching-identifiers-hash the-text the-start-pos the-end-pos
                                                             include-require-arrows?)
          (define binding-arrows '())
          (define (add-binding-arrow arr)
            (when (or include-require-arrows?
                      (not (var-arrow-require-arrow? arr)))
              (set! binding-arrows (cons arr binding-arrows))))
          (if (not the-end-pos)
              (void)
              (for ([the-pos (in-range the-start-pos (+ the-end-pos 1))])
                (define arrs (fetch-arrow-records the-text the-pos))
                (when arrs
                  (for ([arrow (in-list arrs)])
                    (when (var-arrow? arrow)
                      (cond
                        [(and (equal? (var-arrow-start-text arrow) the-text)
                              (<= (var-arrow-start-pos-left arrow) 
                                  the-pos 
                                  (var-arrow-start-pos-right arrow)))
                         ;; a binding occurrence => keep it
                         (add-binding-arrow arrow)]
                        [else
                         ;; a bound occurrence => find binders
                         (for ([candidate-binder 
                                (in-list (fetch-arrow-records (var-arrow-start-text arrow)
                                                              (var-arrow-start-pos-left arrow)))])
                           (when (var-arrow? candidate-binder)
                             (when (and (equal? (var-arrow-start-text arrow) 
                                                (var-arrow-start-text candidate-binder))
                                        (equal? (var-arrow-start-pos-left arrow)
                                                (var-arrow-start-pos-left candidate-binder))
                                        (equal? (var-arrow-start-pos-right arrow)
                                                (var-arrow-start-pos-right candidate-binder)))
                               (add-binding-arrow candidate-binder))))]))))))
          
          (define identifiers-hash #f)
          (define (add-one txt start end)
            (hash-set! identifiers-hash (list txt start end) #t))
          (define (get-identifiers-hash)
            (unless identifiers-hash
              (set! identifiers-hash (make-hash))
              (define already-considered (make-hash))
              (for ([binding-arrow (in-list binding-arrows)])
                (add-one (var-arrow-start-text binding-arrow)
                         (var-arrow-start-pos-left binding-arrow)
                         (var-arrow-start-pos-right binding-arrow))
                (define range-to-consider
                  (cons (var-arrow-start-pos-left binding-arrow)
                        (var-arrow-start-pos-right binding-arrow)))
                (unless (hash-ref already-considered range-to-consider #f)
                  (hash-set! already-considered range-to-consider #t)
                  (for ([pos (in-range (car range-to-consider) (cdr range-to-consider))])
                    (for ([arrow (in-list (fetch-arrow-records 
                                           (var-arrow-start-text binding-arrow)
                                           pos))])
                      (when (var-arrow? arrow)
                        (when (or include-require-arrows?
                                  (not (var-arrow-require-arrow? arrow)))
                          (when (and (equal? (var-arrow-start-text arrow)
                                             (var-arrow-start-text binding-arrow))
                                     (equal? (var-arrow-start-pos-left arrow)
                                             (var-arrow-start-pos-left binding-arrow))
                                     (equal? (var-arrow-start-pos-right arrow)
                                             (var-arrow-start-pos-right binding-arrow)))
                            (add-one (var-arrow-end-text arrow)
                                     (var-arrow-end-pos-left arrow)
                                     (var-arrow-end-pos-right arrow))))))))))
            identifiers-hash)
          (if (not the-end-pos)
              (values null null)
              (values binding-arrows get-identifiers-hash)))
        
        ;; sort-and-merge : (listof (cons number number)) -> (listof (cons number number))
        ;; the result is guaranteed to be non-overlapping ranges, 
        ;; sorted from smallest to largest
        (define (sort-and-merge start+ends)
          (define sorted-positions (sort start+ends < #:key car))
          (let loop ([positions sorted-positions])
            (cond
              [(null? positions) '()]
              [(null? (cdr positions)) positions]
              [else
               (define fst (car positions))
               (define snd (cadr positions))
               (cond
                 [(<= (cdr fst) (car snd)) ;; no overlap
                  (cons fst (loop (cdr positions)))]
                 [else
                  (define merged (cons (car fst) (max (cdr fst) (cdr snd))))
                  (loop (cons merged (cddr positions)))])])))
        
        ;; find-name-to-offer : (non-empty-listof identifier?) -> string?
        (define/private (find-name-to-offer binding-var-arrows)
          (define longest-var-arrow
            (car 
             (sort binding-var-arrows
                   >
                   #:key (λ (x) (- (var-arrow-start-pos-right x)
                                   (var-arrow-start-pos-left x))))))
          (send (var-arrow-start-text longest-var-arrow)
                get-text
                (var-arrow-start-pos-left longest-var-arrow)
                (var-arrow-start-pos-right longest-var-arrow)))
        
        ;; find-parent : menu-item-container<%> -> (union #f (is-a?/c top-level-window<%>)
        (define/private (find-menu-parent menu)
          (let loop ([menu menu])
            (cond
              [(is-a? menu menu-bar%) (send menu get-frame)]
              [(is-a? menu popup-menu%)
               (let ([target (send menu get-popup-target)])
                 (cond
                   [(is-a? target editor<%>) 
                    (let ([canvas (send target get-canvas)])
                      (and canvas
                           (send canvas get-top-level-window)))]
                   [(is-a? target window<%>) 
                    (send target get-top-level-window)]
                   [else #f]))]
              [(is-a? menu menu-item<%>) (loop (send menu get-parent))]
              [else #f])))
        
        
        ;;;;;;;;;;;Rename Menu callback
        
        (define (rename-menu-callback make-identifiers-hash name-to-offer
                                      binding-identifiers parent text)              
          (define (name-dup? x) 
            (for/or ([var-arrow (in-list binding-identifiers)])
              ((var-arrow-name-dup? var-arrow) x)))
          (define new-str
            (fw:keymap:call/text-keymap-initializer
             (λ ()
               (get-text-from-user
                (string-constant cs-rename-id)
                (fw:gui-utils:format-literal-label (string-constant cs-rename-var-to) 
                                                   name-to-offer)
                parent
                name-to-offer
                #:dialog-mixin frame:focus-table-mixin))))
          (when new-str
            (define new-sym (format "~s" (string->symbol new-str)))
            (define dup-name? (name-dup? new-sym))
            ;;;changes
            (define imported? #f)
            ;;; end change
            (define do-renaming?
              (or (not dup-name?)
                  (equal?
                   (message-box/custom
                    (string-constant check-syntax)
                    (fw:gui-utils:format-literal-label
                     (string-constant cs-name-duplication-error) 
                     new-sym)
                    (string-constant cs-rename-anyway)
                    (string-constant cancel)
                    #f
                    parent
                    '(stop default=2)
                    #:dialog-mixin frame:focus-table-mixin)
                   1)))
            
            (when do-renaming?
              (define edit-sequence-txts (list text))
              (define per-txt-positions (make-hash))
              ;;;; CHANGES
              
              (define (normal-rename per-txt-positions)
                (for ([(source-txt start+ends) (in-hash per-txt-positions)])
                  (when (is-a? source-txt text%)
                    (define merged-positions (sort-and-merge start+ends))
                    (send text begin-edit-sequence)
                    (for ([start+end (in-list (reverse merged-positions))])
                      (define start (car start+end))
                      (define end (cdr start+end))
                      (unless (memq source-txt edit-sequence-txts)
                        (send source-txt begin-edit-sequence)
                        (set! edit-sequence-txts (cons source-txt edit-sequence-txts)))
                      (send source-txt delete start end #f)
                      (send source-txt insert new-sym start start #f)))))
              
              
              
              
              (for ([(k _) (in-hash (make-identifiers-hash))])
                (define-values (txt start-pos end-pos) (apply values k))
                (hash-set! per-txt-positions txt 
                           (cons (cons start-pos end-pos)
                                 (hash-ref per-txt-positions txt '()))))
              
              
              (normal-rename per-txt-positions)
              #;(send text end-edit-sequence)
              (for ([txt (in-list edit-sequence-txts)])
                (send txt end-edit-sequence)))))
        ;;;; Wide Scope Replacement
        (define/private (wide-scope-replacement make-identifiers-hash binding-identifiers parent text start-selection end-selection binding-aux #:python? [python? #f])
          (set! wideScopeDetected #f)
          (define-values (callmethod bodymethod) (extract-function make-identifiers-hash binding-identifiers
                                               parent text start-selection end-selection binding-aux #:python? python? #:wide-scope? #t))
          

          (displayln "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
          (displayln callmethod)
          (displayln "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
          (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:detect-refactorings #f
                              #:check-refactorings #t #:call-method callmethod #:body-method bodymethod #:parent parent))
        
        ;;;; Extract function
        ;; callback for the Added-menu Extract Method
        (define/private (extract-function make-identifiers-hash binding-identifiers parent text start-selection
                                          end-selection binding-aux #:python? [python? #f] #:wide-scope? [wide-scope #f])
          (define (name-dup? x) 
            (for/or ([var-arrow (in-list binding-identifiers)])
              ((var-arrow-name-dup? var-arrow) x)))
          (define new-str
            (fw:keymap:call/text-keymap-initializer
             (λ ()
               (get-text-from-user
                (fw:gui-utils:format-literal-label "Extract Function")
                (fw:gui-utils:format-literal-label "Name of the new function")
                parent
                (fw:gui-utils:format-literal-label "name") ;check if name colides?? 
                #:dialog-mixin frame:focus-table-mixin))))
          
          
          (when new-str
            (define new-sym (format "~s" (string->symbol new-str)))
            (define dup-name? (name-dup? new-sym))
            
            ;;check the name of the new function
            (define do-extraction?
              (or (not dup-name?)
                  (equal?
                   (message-box/custom
                    (string-constant check-syntax)
                    (fw:gui-utils:format-literal-label
                     (string-constant cs-name-duplication-error) 
                     new-sym)
                    (string-constant cs-rename-anyway)
                    (string-constant cancel)
                    #f
                    parent
                    '(stop default=2)
                    #:dialog-mixin frame:focus-table-mixin)
                   1)))
            
            ;;actual extract
            (when do-extraction?
              (define edit-sequence-txts (list text))
              (define per-txt-positions (make-hash))
              (define (create-call-method method-name)
                (if python?
                    (string-append method-name "( " (string-append*  (add-between (string-split (get-args)) ",")) ")")
                    (string-append "(" method-name " " (get-args) ")")))
              
              (define (check-arrow var-arrow)
                (define start-selection (send text get-start-position))
                (define end-selection (send text get-end-position))
                (define last-line (send text last-line))
                (displayln last-line)
                ;;find-line uses location, not position. must convert before!
                (define start-box (box 1))
                (define end-box (box 1))
                (define last-box (box 1))
                (send text position-location start-selection #f start-box #t #f #f);Check this!
                (send text position-location end-selection #f end-box #t #f #f);Check this!
                (send text position-location last-line #f last-box #t #f #f) ;Trying
                (define start-line (send text find-line (unbox start-box)))
                (define end-line (send text find-line (unbox end-box)))
                ;;;if from #lang racket or form require
                ;;; transform from position to location
                (define (check-lang var-arrow)
                  ;;first syntatic object (might be good enough)
                  (not (or (eq? (syntax-e (get-syntax-value (var-arrow-start-pos-left var-arrow))) 'racket)
                           (eq? (syntax-e (get-syntax-value (var-arrow-start-pos-left var-arrow))) 'python/lang/python))))
                (define (check-imports var-arrow)
                  ;;;
                  #t)
                (and (check-lang var-arrow) (check-imports var-arrow)))
              
              (define (get-args)
                (define args-pos (list))
                (define args "")
                (define (travel-args) 
                  (for ([var-arrow (in-list binding-aux)]) ;check if the arrow counts,
                    
                    
                    (displayln (var-arrow-level var-arrow))
                    (begin
                      (if (and (or (< (var-arrow-start-pos-left var-arrow) start-selection)
                                   (> (var-arrow-start-pos-right var-arrow) end-selection))
                               (check-arrow var-arrow)) ; check if add args.
                          (begin 
                            (display "get-text value: ")
                            (displayln (send text get-text  (var-arrow-start-pos-left var-arrow) (var-arrow-start-pos-right var-arrow)))
                            
                            (set! args-pos (cons var-arrow args-pos))
                            )
                          (begin
                            (displayln "no args")
                            ;(display "Arrow fail ")
                            ;(displayln var-arrow))
                            )))))
                (travel-args)
                (for ([var-arrow (in-list args-pos)])
                  (begin
                    (set! args (string-append args " " (send text get-text (var-arrow-start-pos-left var-arrow) (var-arrow-start-pos-right var-arrow))))))
                
                (set! args (string-join (remove-duplicates (string-split args)))) ;remove duplicates
                (string-normalize-spaces args)
                (display "args ")
                (displayln args)
                args)
              
              (define (create-method method-body method-name)
                (string-append "\n(define (" method-name " "(get-args) ")" "\n  " method-body ")" ))
              (define (create-method-python method-body method-name)
                (string-append "\ndef " method-name "( " (string-append*  (add-between (string-split (get-args)) ",")) "):\n    " method-body)) ;;missing return
              
              
              ;(displayln "define method-definition")
              (define method-definition (send text get-text start-selection end-selection #t #t))
              
              (define (get-body)
                ;;;TODO implement for python and other languages with returns
                (void))
              
              (if (not wide-scope)
                  (begin
                    (send text begin-edit-sequence)
                    ;(displayln "begin")
                    (unless (memq text edit-sequence-txts)
                      (send text begin-edit-sequence)
                      (set! edit-sequence-txts (cons text edit-sequence-txts)))
                    ;(displayln "before delete")
                    (send text delete start-selection end-selection)
                    ;(displayln "before insert")
                    (send text insert (create-call-method new-str) start-selection)
                    ;(displayln "before move")
                    
                    ;In order to put the new changes in the clipboard
                    ;write the text in the last position of the file, then "cut" it
                    (let ([last-pos (send text last-position)])
                      (if python?
                          (send text insert (create-method-python method-definition new-str) last-pos last-pos)
                          (send text insert (create-method method-definition new-str) last-pos last-pos))
                      (send text cut #f 0 last-pos (send text last-position))) ; time stamp might fail really hard.)
                    (for ([txt (in-list edit-sequence-txts)])
                      (send txt end-edit-sequence))
                    
                    (fw:keymap:call/text-keymap-initializer
                     (λ ()
                       (message-box/custom ;;;Its buggy... fix this
                        (fw:gui-utils:format-literal-label "Title Extract-Function End")
                        (fw:gui-utils:format-literal-label "The Extracted Function is now on your Clipboard")
                        (fw:gui-utils:format-literal-label "Ok")
                        ;(string-constant cancel)
                        #f ;#f means that we are not using this button, maximum is 3 button
                        #f
                        parent
                        '(caution default=1)
                        #:dialog-mixin frame:focus-table-mixin))))
                  (values (string-append (create-call-method new-str) "\n") (create-method method-definition new-str)))))) ;;if wide-scope return the call method!
        
        ;;;;; end refactoring functions
        
        (define (get-syntax-value start-position #:expanded? [expanded? #f])
          (define start-selection (send text get-start-position))
          (define end-selection (send text get-end-position))
          (define last-line (send text last-line))
          (displayln last-line)
          ;;find-line uses location, not position. must convert before!
          (define start-box (box 1))
          (define end-box (box 1))
          (define last-box (box 1))
          (send text position-location start-selection #f start-box #t #f #f);Check this!
          (send text position-location end-selection #f end-box #t #f #f);Check this!
          (send text position-location last-line #f last-box #t #f #f) ;Trying
          (send text position-location start-position #f start-box #t #f #f);Check this!
          (define start-line (send text find-line (unbox start-box)))
          (define end-line (send text find-line (unbox end-box)))
          (define last-line-pos (send text find-line (unbox last-box)))
          (if expanded?
              (code-walker expanded-program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line))
              (code-walker-non-expanded non-expanded-program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line))))
        
        (define (printPython tab interactions expanded? #:print-expanded-form? [expanded-form? #f])
          (define start-selection (send text get-start-position))
          (define end-selection (send text get-end-position))
          (define last-line (send text last-line))
          (displayln last-line)
          ;;find-line uses location, not position. must convert before!
          (define start-box (box 1))
          (define end-box (box 1))
          (define last-box (box 1))
          (send text position-location start-selection #f start-box #t #f #f);Check this!
          (send text position-location end-selection #f end-box #t #f #f);Check this!
          (send text position-location last-line #f last-box #t #f #f) ;Trying
          (displayln last-box)
          (displayln (send text position-line last-line))
          (define start-line (send text find-line (unbox start-box)))
          (define end-line (send text find-line (unbox end-box)))
          (define last-line-pos (send text find-line (unbox last-box)))
          (if expanded-form?
              (with-lock/edit-sequence
               text
               (λ ()
                 (drracket:eval:expand-program
                  #:gui-modules? #f
                  (drracket:language:make-text/pos text
                                                   0
                                                   (send text last-position)) ;;Input
                  settings
                  #f;(not module-language?)
                  init-proc
                  kill-termination
                  (λ (sexp loop) ; =user=
                    (cond
                      [(eof-object? sexp)
                       (set! normal-termination? #t)
                       (parameterize ([current-eventspace drs-eventspace])
                         (queue-callback
                          (λ () ; =drs=
                            (with-lock/edit-sequence
                             definitions-text
                             (λ ()
                               (parameterize ([current-annotations definitions-text])
                                 (begin
                                   (expansion-completed)))))
                            (cleanup)
                            (custodian-shutdown-all user-custodian))))]
                      [else
                       (unless module-language?
                         (eval-compile-time-part-of-top-level sexp))
                       (parameterize ([current-eventspace drs-eventspace])
                         (queue-callback
                          (λ () ; =drs=
                            (with-lock/edit-sequence
                             definitions-text
                             (λ ()
                               #;(update-status-line 
                                  'drracket:check-syntax:status status-coloring-program)
                               (parameterize ([current-annotations definitions-text])
                                 (begin
                                   (print-languages-syntax sexp #t (get-definitions-text) start-selection end-selection start-line end-line last-line)
                                   #;(parameterize ((print-syntax-width 9000))
                                       (displayln sexp))
                                   (expanded-expression sexp)))
                               #;(close-status-line 'drracket:check-syntax:status))))))
                       (loop)])))))
              ((λ ()
                 ((drracket:eval:traverse-program/multiple
                   #:gui-modules? #f
                   settings
                   init-proc
                   kill-termination)
                  (drracket:language:make-text/pos text
                                                   0
                                                   (send text last-position))
                  (λ (sexp loop) ;this is the "iter"
                    (cond
                      [(eof-object? sexp)
                       (custodian-shutdown-all user-custodian)]
                      [else
                       (displayln sexp)
                       (parameterize ((print-syntax-width 9000))
                         (displayln sexp))
                       (displayln non-expanded-program)
                       (print-languages-syntax sexp #f (get-definitions-text) start-selection end-selection
                                               start-line end-line last-line)
                       
                       (loop)])) 
                  #t)))))
        
        
        (define (refactoring-syntax tab interactions refactoring? #:print-extra-info? [print-extra-info? #f] #:auto-refactoring [auto-refactoring? #f] 
                                    #:detect-refactorings [detect-refactorings? #f]
                                    #:get-refactoring-string [get-refactoring-string #f] #:check-refactorings [check-refactorings? #f]
                                    #:call-method [call-method ""] #:body-method[body-method ""] #:parent [parent null])
          
          
          #;(dump-arrow-records)
          (displayln "end arrow")
          (define interactions-text interactions)
          ;;;;;;;;;;;;;;;;;; Editor information
          (define start-selection (send text get-start-position))
          (define end-selection (send text get-end-position))
          (define last-line (send text last-line))
          (displayln last-line)
          ;;find-line uses location, not position. must convert before!
          (define start-box (box 1))
          (define end-box (box 1))
          (define last-box (box 1))
          (send text position-location start-selection #f start-box #t #f #f);Check this!
          (send text position-location end-selection #f end-box #t #f #f);Check this!
          (send text position-location last-line #f last-box #t #f #f) ;Trying
          (displayln last-box)
          (displayln (send text position-line last-line))
          (define start-line (send text find-line (unbox start-box)))
          (define end-line (send text find-line (unbox end-box)))
          (define last-line-pos (send text find-line (unbox last-box)))
          (displayln last-line-pos)
          (displayln end-line)
          ;;;;;;;;;;;;;;;;;;
          (define (check-similar start-line)
            (displayln "check-similar")
            (define (search-similiar sexp randombool text start-selection end-selection 
                                     start-line end-line last-line check-refactorings?)
              (displayln "search-similar")
              (define (compare-syntax userSelected autoSelected)
                ;;; add a boolean that compares values or not
                ;;; end cases, a #f or userStack and autoStack both reach null at the same time
                (displayln "compare-syntax")
                (define userStack null)
                (define autoStack null)
                (define (compare-aux userSelected autoSelected)
                  (cond [(or (null? userSelected) (null? autoSelected))
                         (displayln "It's null")
                         (if (and (null? userSelected) (null? autoSelected))
                             (cond [(and (null? userStack) (null? autoStack)) #t]
                                   #;[(and (identifier? userStack) (identifier? autoStack))]
                                   [(and (pair? userStack) (pair? autoStack))
                                    (set! userSelected (car userStack))
                                    (set! autoSelected (car autoStack))
                                    (set! userStack (cdr userStack))
                                    (set! autoStack (cdr autoStack))
                                    (compare-aux userSelected autoSelected)]
                                   [(and (syntax? userStack) (syntax? autoStack))
                                    (set! userSelected (syntax-e userStack))
                                    (set! autoSelected (syntax-e autoStack))
                                    (set! userStack (list))
                                    (set! autoStack (list))]
                                   [else #f])
                             #f)]
                        #;[stop? (displayln "evaluation stopped")]
                        [(or (identifier? userSelected) (identifier? autoSelected))
                         (displayln "identifier!")
                         (if (and (identifier? userSelected) (identifier? autoSelected))
                             (if (free-identifier=? userSelected autoSelected #f #f) ;;check this
                                 ;;not sure if any stack is null
                                 (begin 
                                   (displayln "FREE-IDENTIFIER")
                                   (displayln (identifier-binding userSelected #f))
                                   (displayln (identifier-binding autoSelected #f))
                                   (displayln userSelected)
                                   (displayln autoSelected)
                                   
                                   (cond [(and (null? userStack) (null? autoStack)) #t]
                                         [(null? userStack) #f]
                                         [(null? autoStack) #f]
                                         [(and (pair? userStack) (pair? autoStack))
                                          (set! userSelected (car userStack))
                                          (set! autoSelected (car autoStack))
                                          (set! userStack (cdr userStack))
                                          (set! autoStack (cdr autoStack))
                                          (compare-aux userSelected autoSelected)]
                                         [(and (syntax? userStack) (syntax? autoStack))
                                          (displayln "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
                                          (displayln "this should not happen!!!!!!!!!!!!!!!!!!!!!!!!!!")
                                          (set! userSelected (syntax-e userSelected))
                                          (set! autoSelected (syntax-e autoStack))
                                          (set! userStack (list))
                                          (set! autoStack (list))
                                          (compare-aux userSelected autoSelected)]
                                         [else #f]))
                                 #f)
                             #f)]
                        [(or (pair? userSelected) (pair? autoSelected))
                         (displayln "It's pair")
                         (if (and (pair? userSelected) (pair? autoSelected))
                             (begin
                               (set! userStack (cons (cdr userSelected) userStack)) ;;add to stack
                               (set! userSelected (car userSelected))
                               (set! autoStack (cons (cdr autoSelected) autoStack)) ;;add to stack
                               (set! autoSelected (car autoSelected))
                               (compare-aux userSelected autoSelected))
                             #f)]
                        [(or (syntax? userSelected) (syntax? autoSelected))
                         (displayln "In Syntax")
                         (if (and (syntax? userSelected) (syntax? autoSelected))
                             (compare-aux (syntax-e userSelected) (syntax-e autoSelected))
                             #f)]
                        [else
                         (displayln "else")
                         (if (equal? userSelected autoSelected)
                             (begin
                               (displayln "equal?")
                               (displayln userSelected)
                               (displayln userStack)
                               (displayln autoSelected)
                               (displayln autoStack)
                               (cond [(and (null? userStack) (null? autoStack)) 
                                      #t]
                                     [(null? userStack) #f]
                                     [(null? autoStack) #f]
                                     [(and (pair? userStack) (pair? autoStack))
                                      (set! userSelected (car userStack))
                                      (set! autoSelected (car autoStack))
                                      (set! userStack (cdr userStack))
                                      (set! autoStack (cdr autoStack))
                                      (compare-aux userSelected autoSelected)]
                                     [(and (syntax? userStack) (syntax? autoStack))
                                      (displayln "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
                                      (displayln "this should not happen!!!!!!!!!!!!!!!!!!!!!!!!!!")
                                      (set! userSelected (syntax-e userSelected))
                                      (set! autoSelected (syntax-e autoStack))
                                      (set! userStack (list))
                                      (set! autoStack (list))
                                      (compare-aux userSelected autoSelected)]
                                     [else #f]))
                             #f)]))
                (compare-aux userSelected autoSelected))
              
              ;;;from the selected argument try and extract a "form"
              ;;;get-selected argument
              (define program sexp)
              (displayln "before code-walker")
              (define selected-expressions (code-walker program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line)))
              (displayln "after code-walker")
              (define (highlight-selected aux-sexp)
                (define (replace-expressions stx-highlight)
                  ;;receive function call already computed
                  ;; remove syntax-span stx-hihlight
                  ;; (- (syntax-position stx-highlight) 1)  intial
                  ;; (+ (syntax-span stx-highlight) (syntax-position stx-highlight)) last
                  ;; insert -> start from the end to the beginning (Hopefully is how it is implemented)

                  #;(send text begin-edit-sequence)
                    ;(displayln "begin")
                    #;(unless (memq text edit-sequence-txts)
                      (send text begin-edit-sequence)
                      (set! edit-sequence-txts (cons text edit-sequence-txts)))
                    ;(displayln "before delete")
                    (send text delete (- (syntax-position stx-highlight) 1) (+ (syntax-span stx-highlight) (syntax-position stx-highlight)))
                    ;(displayln "before insert")
                    (send text insert call-method (- (syntax-position stx-highlight) 1))
                    ;(displayln "before move")
                    
                    ;In order to put the new changes in the clipboard
                    ;write the text in the last position of the file, then "cut" it
                    (let ([last-pos (send text last-position)])
                      #;(if python?
                          (send text insert (create-method-python method-definition new-str) last-pos last-pos)
                          (send text insert (create-method method-definition new-str) last-pos last-pos))
                      (send text cut #f 0 last-pos (send text last-position))) ; time stamp might fail really hard.)
                    #;(for ([txt (in-list edit-sequence-txts)])
                      (send txt end-edit-sequence))
                    
                    #;(fw:keymap:call/text-keymap-initializer
                     (λ ()
                       (message-box/custom ;;;Its buggy... fix this
                        (fw:gui-utils:format-literal-label "Title Extract-Function End")
                        (fw:gui-utils:format-literal-label "The Extracted Function is now on your Clipboard")
                        (fw:gui-utils:format-literal-label "Ok")
                        ;(string-constant cancel)
                        #f ;#f means that we are not using this button, maximum is 3 button
                        #f
                        parent
                        '(caution default=1)
                        #:dialog-mixin frame:focus-table-mixin))))
                
                (displayln "before traverse")
                (displayln (length aux-sexp))
                (define (highlight-selected-aux aux-sexp sexp)
                  (unless (null? aux-sexp)
                    (set! wideScopeDetected #t))
                  (send text unhighlight-ranges/key 'key)
                  (let loop ((aux-sexp aux-sexp)
                             (sexp sexp))
                    
                    (unless (null? aux-sexp)
                      (displayln "painting!")
                      (let ((stx-highlight (code-walker-non-expanded sexp (syntax-line (car aux-sexp)) (+ 1 last-line) (+ 1 last-line))))
                        (if (string=? call-method "")
                            (send text highlight-range
                                  (- (syntax-position stx-highlight) 1)
                                  (+ (syntax-span stx-highlight) (syntax-position stx-highlight))
                                  (make-object color% 0 255 0 0.35) #:key 'key)
                            (if (null? (cdr aux-sexp))
                                (begin
                                  (replace-expressions stx-highlight)
                                  (let ([last-pos (send text last-position)])
                          (send text insert body-method last-pos last-pos)
                          ;(send text insert (create-method method-definition new-str) last-pos last-pos))
                      (send text cut #f 0 last-pos (send text last-position))) ; time stamp might fail really hard.)
                    #;(for ([txt (in-list edit-sequence-txts)])
                      (send txt end-edit-sequence))
                    
                    (fw:keymap:call/text-keymap-initializer
                     (λ ()
                       (message-box/custom ;;;Its buggy... fix this
                        (fw:gui-utils:format-literal-label "Title Extract-Function End")
                        (fw:gui-utils:format-literal-label "The Extracted Function is now on your Clipboard")
                        (fw:gui-utils:format-literal-label "Ok")
                        ;(string-constant cancel)
                        #f ;#f means that we are not using this button, maximum is 3 button
                        #f
                        parent
                        '(caution default=1)
                        #:dialog-mixin frame:focus-table-mixin)))

                                  )
                             (replace-expressions stx-highlight))))
                      
                      (loop (cdr aux-sexp) sexp))))
                ((λ ()
                   ((drracket:eval:traverse-program/multiple
                     #:gui-modules? #f
                     settings
                     void
                     void)
                    (drracket:language:make-text/pos text
                                                     0
                                                     (send text last-position))
                    (λ (sexp loop) ;this is the "iter"
                      (cond
                        [(eof-object? sexp)
                         (custodian-shutdown-all user-custodian)]
                        [else
                         (displayln "else painting")
                         (highlight-selected-aux aux-sexp sexp)
                         (loop)])) 
                    #t))))
              (define to-highlight-list (list))
              (define (search-refactorings start)
                (displayln "search-refactorings!")
                (define aux (code-walker program (+ 1 start) (+ 1 last-line) (+ 1 last-line)))
                ;;; aux is the form to compare with
                ;;; selected-expression is the form have
                (displayln "compare syntax")
                (define result (compare-syntax selected-expressions aux))
                (when result
                  ;;; there is a match
                  (displayln "match found!!")
                  ;;;; (get list of positions, put that on a list)
                  ;;;;; in the end call specialized function, paint all of the list
                  
                  #;(parameterize ((print-syntax-width 9000))
                      (displayln selected-expressions)
                      (displayln (string-length (syntax->string  selected-expressions)))
                      (displayln (cdr (syntax-e (cdr (syntax-e selected-expressions)))))
                      (displayln (string-length (syntax->string
                                                 (car (cdr (syntax-e (cdr (syntax-e selected-expressions))))))))
                      (displayln (string-length (syntax->string
                                                 (car (cdr (cdr (syntax-e (cdr (syntax-e selected-expressions))))))))))
                  (set! to-highlight-list (cons aux to-highlight-list))
                  
                  #;(send text highlight-range (syntax-position selected-expressions) 
                          (+ 2 (string-length (syntax->string aux)) (syntax-position selected-expressions)) (make-object color% 0 255 0 0.35) #:key 'key)
                  #;(send text highlight-range (syntax-position aux) 
                          (+ 2 (string-length (syntax->string aux)) (syntax-position aux)) (make-object color% 0 200 0 0.25) #:key 'key))
                #;(send text unhighlight-range start end color [caret-space style])
                ;;;;; highlight/display (end start color)
                #;(send text highlight-range 1 5 (make-object color% 255 0 0 1.0))
                
                (displayln "loop")
                (displayln aux)
                (if (> start (+ last-line 1))
                    (begin
                      (displayln "Over")
                      (displayln start)
                      (displayln last-line)
                      ;;;call the function to highlight all the necessary areas :D
                      (displayln to-highlight-list)
                      #;(read)
                      (highlight-selected to-highlight-list))
                    (begin
                      (when (= start last-line)
                        (displayln "last one")
                        (displayln start)
                        (displayln last-line)
                        #;(read))
                      (search-refactorings (+ 1 start)))))
              (displayln "before call")
              (search-refactorings start-line))
            (define (search-similar-aux)  ;;Search used in Wide-Scope-Replacement
              (with-lock/edit-sequence
               text
               (λ ()
                 (drracket:eval:expand-program
                  #:gui-modules? #f
                  (drracket:language:make-text/pos text
                                                   0
                                                   (send text last-position)) ;;Input
                  settings
                  #f;(not module-language?)
                  init-proc
                  kill-termination
                  (λ (sexp loop) ; =user=
                    (cond
                      [(eof-object? sexp)
                       (set! normal-termination? #t)
                       (parameterize ([current-eventspace drs-eventspace])
                         (queue-callback
                          (λ () ; =drs=
                            (with-lock/edit-sequence
                             definitions-text
                             (λ ()
                               (parameterize ([current-annotations definitions-text])
                                 (begin
                                   (expansion-completed)))))
                            (cleanup)
                            (custodian-shutdown-all user-custodian))))]
                      [else
                       (unless module-language?
                         (eval-compile-time-part-of-top-level sexp))
                       (parameterize ([current-eventspace drs-eventspace])
                         (queue-callback
                          (λ () ; =drs=
                            (with-lock/edit-sequence
                             definitions-text
                             (λ ()
                               #;(update-status-line 
                                  'drracket:check-syntax:status status-coloring-program)
                               (displayln "?????????????????????? expanding program ??????????????????")
                               (unless (= start-selection end-selection)
                                 (search-similiar sexp #f text start-selection end-selection 
                                                  start-line last-line last-line check-refactorings?)))))))
                       (loop)]))))))
            
            #;(define (search-similar-aux)
                ((λ ()
                   ((drracket:eval:traverse-program/multiple
                     #:gui-modules? #f
                     settings
                     init-proc
                     kill-termination)
                    (drracket:language:make-text/pos text
                                                     0
                                                     (send text last-position))
                    (λ (sexp loop) ;this is the "iter"
                      (cond
                        [(eof-object? sexp)
                         (custodian-shutdown-all user-custodian)]
                        [else
                         (displayln sexp)
                         (search-similiar sexp #f text start-selection end-selection 
                                          start-line last-line last-line check-refactorings?)
                         (parameterize ((print-syntax-width 9000))
                           (displayln sexp))
                         (displayln not-expanded-program)
                         (loop)])) 
                    #t))))
            (send clear enable #t)
            (search-similar-aux))
          
          (define (automated-refactoring start-line #:text-location [text-location (drracket:language:make-text/pos text
                                                                                                                    0
                                                                                                                    (send text last-position))])
            (send text begin-edit-sequence)
            (send text insert " " (send text last-position))
            (send text delete (- (send text last-position) 1) (send text last-position))
            (send text end-edit-sequence)
            ((λ ()
               ((drracket:eval:traverse-program/multiple
                 #:gui-modules? #f
                 settings
                 init-proc
                 kill-termination)
                #;(drracket:language:make-text/pos text
                                                   0
                                                   (send text last-position))
                text-location
                (λ (sexp loop) ;this is the "iter"
                  (cond
                    [(eof-object? sexp)
                     (custodian-shutdown-all user-custodian)]
                    [else
                     (displayln sexp)
                     (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line last-line last-line auto-refactoring? detect-refactorings? get-refactoring-string)
                     (parameterize ((print-syntax-width 9000))
                       (displayln sexp))
                     (displayln non-expanded-program)
                     (loop)])) 
                #t)))
            (unless (>= start-line last-line)
              (automated-refactoring (+ 1 start-line)))
            (send text begin-edit-sequence)
            (send text insert " " (send text last-position))
            (send text delete (- (send text last-position) 1) (send text last-position))
            (send text end-edit-sequence))
          
          
          (when auto-refactoring?
            (automated-refactoring 0))
          (when check-refactorings?
            (check-similar start-line))
          
          (unless (or get-refactoring-string auto-refactoring? check-refactorings?)
            (if refactoring?
                (with-lock/edit-sequence
                 text
                 (λ ()
                   (drracket:eval:expand-program
                    #:gui-modules? #f
                    (drracket:language:make-text/pos text
                                                     0
                                                     (send text last-position)) ;;Input
                    settings
                    #f;(not module-language?)
                    init-proc
                    kill-termination
                    (λ (sexp loop) ; =user=
                      (cond
                        [(eof-object? sexp)
                         (set! normal-termination? #t)
                         (parameterize ([current-eventspace drs-eventspace])
                           (queue-callback
                            (λ () ; =drs=
                              (with-lock/edit-sequence
                               definitions-text
                               (λ ()
                                 (parameterize ([current-annotations definitions-text])
                                   (begin
                                     (expansion-completed)))))
                              (cleanup)
                              (custodian-shutdown-all user-custodian))))]
                        [else
                         (unless module-language?
                           (eval-compile-time-part-of-top-level sexp))
                         (parameterize ([current-eventspace drs-eventspace])
                           (queue-callback
                            (λ () ; =drs=
                              (with-lock/edit-sequence
                               definitions-text
                               (λ ()
                                 #;(update-status-line 
                                    'drracket:check-syntax:status status-coloring-program)
                                 (parameterize ([current-annotations definitions-text])
                                   (begin
                                     #;(syntax-refactoring sexp #t (get-definitions-text) start-selection end-selection start-line end-line last-line (get-editor%))
                                     (parameterize ((print-syntax-width 9000))
                                       (displayln sexp))
                                     (expanded-expression sexp)))
                                 #;(close-status-line 'drracket:check-syntax:status))))))
                         (loop)])))))
                ((λ ()
                   ((drracket:eval:traverse-program/multiple
                     #:gui-modules? #f
                     settings
                     init-proc
                     kill-termination)
                    (drracket:language:make-text/pos text
                                                     0
                                                     (send text last-position))
                    (λ (sexp loop) ;this is the "iter"
                      (cond
                        [(eof-object? sexp)
                         (custodian-shutdown-all user-custodian)]
                        [else
                         (displayln sexp)
                         (parameterize ((print-syntax-width 9000))
                           (displayln sexp))
                         (displayln non-expanded-program)
                         (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line end-line last-line auto-refactoring? detect-refactorings? get-refactoring-string)
                         
                         (loop)])) 
                    #t))))
            (send text begin-edit-sequence)
            (send text insert " " (send text last-position))
            (send text delete (- (send text last-position) 1) (send text last-position))
            (send text end-edit-sequence))
          
          (when get-refactoring-string
            (let ((result "empty" )
                  (tID null))
              ((λ ()
                 ((drracket:eval:traverse-program/multiple
                   #:gui-modules? #f
                   settings
                   init-proc
                   kill-termination)
                  (drracket:language:make-text/pos text
                                                   0
                                                   (send text last-position))
                  (λ (sexp loop) ;this is the "iter"
                    (cond
                      [(eof-object? sexp)
                       (custodian-shutdown-all user-custodian)]
                      [else
                       (displayln "#####################")
                       (displayln loop)
                       (parameterize ((print-syntax-width 9000))
                         (displayln "#####################")
                         (displayln loop)
                         (displayln sexp))
                       #;(displayln (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line end-line last-line auto-refactoring? detect-refactorings? get-refactoring-string))
                       (set! result (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line end-line last-line auto-refactoring? detect-refactorings? get-refactoring-string))
                       (loop)])) 
                  #t)))
              (displayln result)
              result)))
        (define refactoring-menu  (new menu% [label "Refactoring Menu"] [parent (send (send (send (get-definitions-text) get-tab) get-frame) get-menu-bar)]))
        (append-editor-operation-menu-items refactoring-menu #t)
        (set! detect 
              (make-object menu-item%
                ;(get-refactoring-string)
                "Detect Refactorings"
                refactoring-menu
                (λ (item evt)
                  (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:detect-refactorings #t))))
        
        ;;;;; Wide-Scope-Replacement
        #;(set! checkRefactoring
              (make-object menu-item%
                ;(get-refactoring-string)
                "Check Refactoring"
                refactoring-menu
                (λ (item evt)
                  (if wideScopeDetected
                      (begin
                        (displayln "wide Scope calling")
                        #;(wide-scope-replacement make-identifiers-hash binding-identifiers parent text start-selection end-selection binding-aux #:python? [python? #f]))
                      (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:detect-refactorings #f #:check-refactorings #t)))))
        #;(set! wideScope
                (make-object menu-item%
                  ;(get-refactoring-string)
                  "Do Refactoring"
                  refactoring-menu
                  (λ (item evt)
                    (if wideScopeDetected
                        (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:detect-refactorings #f #:check-refactorings #t))
                    (void))))
        #;(send wideScope enable #f)
        (set! clear 
              (make-object menu-item%
                ;(get-refactoring-string)
                "Clear Refactorings"
                refactoring-menu
                (λ (item evt)
                  (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:detect-refactorings #t))))
        (send clear enable #f)
        
        (set! AutomaticRefactoring 
              (make-object menu-item%
                ;(get-refactoring-string)
                "Automatic Refactoring"
                refactoring-menu
                (λ (item evt)
                  (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:auto-refactoring #t))))
        
        (send AutomaticRefactoring enable #t)
        
        (define (create-refactoring-menu menu bool pos text)
          
          (define need-a-sep? (not #f))
          (define (add-sep) 
            (unless need-a-sep? 
              (set! need-a-sep? #t)
              (when need-a-sep?
                (new separator-menu-item% [parent menu]))))
          (define refactoring-menu
            (make-object menu%
              "Refactoring Operations"
              menu))
          (set! RefactoringOperations 
                (make-object menu-item%
                  "Refactoring operations"
                  refactoring-menu
                  (λ (item evt)
                    (refactoring-syntax (get-current-tab) (get-interactions-text) #f))))
          (set! printPythonmenu 
                (make-object menu-item%
                  "Print non expanded form"
                  refactoring-menu
                  (λ (item evt)
                    (printPython (get-current-tab) (get-interactions-text) #f))))
          (add-sep)
          (make-object menu-item%
            "Print expanded form"
            refactoring-menu
            (λ (item evt)
              (printPython (get-current-tab) (get-interactions-text) #t #:print-expanded-form? #t)))
          
          ;;;;aux functions
          (define start-selection (send (get-definitions-text) get-start-position))
          (define end-selection (send (get-definitions-text) get-end-position))
          (define-values (binding-identifiers make-identifiers-hash)
            (position->matching-identifiers-hash text pos pos #t))
          ;I have access to the binding-identifiers and the make-identifiers-hash
          (define-values (binding-aux make-identifiers-aux) ;the binding idenfitiers of the selection
            (position->matching-identifiers-hash text start-selection end-selection #t))
          
          (define (compute-imports) ;No imports?
            (displayln start-selection)
            (displayln end-selection)
            (set! binding-aux (remove-duplicates binding-aux = #:key(lambda (x) (var-arrow-end-pos-left x))))
            ;(display binding-aux)
            ;(displayln "teste")
            (let ([return #f])
              (for ([var-arrow (in-list binding-aux)]) ;check if the arrow is "import" or "require" 
                (begin
                  (let ([name (send text get-text  (var-arrow-end-pos-left var-arrow) (var-arrow-end-pos-right var-arrow))])
                    
                    (when (and (or (and (>= (var-arrow-end-pos-left var-arrow) start-selection)
                                        (<= (var-arrow-end-pos-right var-arrow) end-selection))
                                   (and (>= (var-arrow-start-pos-left var-arrow) start-selection)
                                        (<= (var-arrow-start-pos-right var-arrow) end-selection))
                                   )
                               (or (string=? name "import") 
                                   (string=? name "require" )))
                      (begin
                        ;(displayln name)
                        (set! return #t))  ; check if add args.
                      ))))
              ;(displayln return)
              return))
          
          ;;;;
          (unless (null? binding-identifiers)
            (define name-to-offer (find-name-to-offer binding-identifiers))
            (new menu-item%
                 [parent menu]
                 [label (fw:gui-utils:format-literal-label (string-constant cs-rename-var)
                                                           name-to-offer)]
                 [callback
                  (λ (x y)
                    (let ([frame-parent (find-menu-parent menu)])
                      (rename-menu-callback make-identifiers-hash
                                            name-to-offer 
                                            binding-identifiers
                                            frame-parent (get-interactions-text ))))]))
          #;(add-sep)
          
          ;;;;; Wide-Scope-Replacement
          (unless (null? binding-aux)
            (set! checkRefactoring
                  (make-object menu-item%
                    ;(get-refactoring-string)
                    "Check Refactoring"
                    refactoring-menu
                    (λ (item evt)
                      (if wideScopeDetected
                          (begin
                            (displayln "wide Scope calling")
                            (let ([frame-parent (find-menu-parent menu)])
                              (wide-scope-replacement make-identifiers-hash binding-identifiers frame-parent text
                                                      start-selection end-selection binding-aux)))
                          (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:detect-refactorings #f #:check-refactorings #t))))))
          
          ;;;;; Extract Function
          (unless (null? binding-aux) ;binding-aux check this!
            ;(define name-to-offer (find-name-to-offer binding-identifiers))
            ;;;;aux functions
            (displayln (send text get-text))
            (make-object menu-item%
              "Extract Function"
              ;menu
              refactoring-menu
              (λ (item evt)
                (let ([frame-parent (find-menu-parent menu)])
                  ; (what-is? menu) is an editor
                  (displayln start-selection)
                  (displayln end-selection)
                  (extract-function make-identifiers-hash binding-identifiers
                                    frame-parent text start-selection end-selection binding-aux)))))
          ;;;;;; Python Extract Function
          #;(make-object menu-item%
              "Extract Function"
              menu
              (λ (item evt)
                (let ([frame-parent (find-menu-parent menu)])
                  ; (what-is? menu) is an editor
                  (displayln start-selection)
                  (displayln end-selection)
                  (extract-function make-identifiers-hash binding-identifiers
                                    frame-parent text start-selection end-selection binding-aux #:python? #t))))
          
          (displayln "$$$$$$$$$$$$$$$$$$$$$$$$$$ LABEL")
          (displayln (send RefactoringOperations get-label))
          (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:get-refactoring-string #t)
          (displayln "@@@@@@")
          (displayln "end")
          #;(void))
        (keymap:add-to-right-button-menu/before
         (let ([old (keymap:add-to-right-button-menu/before)])
           (λ (menu editor event)
             (old menu editor event)
             (define-values (pos text) (send (get-definitions-text) get-pos/text event))
             
             (create-refactoring-menu menu #f pos text))))
        #;(keymap:add-to-right-button-menu/before
           (let ([old (keymap:add-to-right-button-menu/before)])
             (λ (menu editor event)
               (old menu editor event)
               (define-values (pos text) (send (get-definitions-text) get-pos/text event))
               
               (create-refactoring-menu menu #f pos text))))
        (define (auto-tests)
          #;(message-box "test 1" "Running tests here?")
          (current-directory "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/")
          #;(define aux (find-files (lambda (a)(string-suffix? (path->string a) ".rkt")) #f))
          (let loop ([aux (find-files (lambda (a)(string-suffix? (path->string a) ".in")) #f)])
            (if (null? aux)
                (displayln "end")
                (begin
                  ((λ ()
                     ((drracket:eval:traverse-program/multiple
                       #:gui-modules? #f
                       settings
                       init-proc
                       kill-termination)
                      #;(drracket:language:make-text/pos text
                                                         0
                                                         (send text last-position))
                      (open-input-file (car aux))
                      (λ (sexp loop) ;this is the "iter"
                        (cond
                          [(eof-object? sexp)
                           (custodian-shutdown-all user-custodian)]
                          [else
                           (displayln sexp)
                           #;(syntax-tests-refactoring sexp start-line)
                           #;(syntax-refactoring sexp #f (get-definitions-text) 0 0 0 20 20 #t #f #f)
                           (let ((arg2 (code-walker-special sexp (+ 1 0) (+ 1 20) (+ 1 20))))
                             (displayln "Inside auto-tests")
                             (define result (racket-parser arg2))
                             (displayln result)
                             (displayln (format "~a" (syntax->datum result)))
                             #;(displayln (file->string "/home/rafaelreia/share/racket/collects/RefactoringTool/AutoTesting/testnot.out"))
                             #;(displayln (file->string (string-append (string-trim (format "~a" (car aux)) ".in") ".out")))
                             #;(displayln "before IF")
                             (if (file-exists? (string-append (string-trim (format "~a" (car aux)) ".in") ".out"))
                                 (if (string=?
                                      (format "~a" (syntax->datum result))
                                      (file->string (string-append (string-trim (format "~a" (car aux)) ".in") ".out")))
                                     (displayln "ITS TRUEEEE")
                                     (begin
                                       (message-box "Racket - Refactoring Tool Error" (string-append "Error in test: " (format "~a" (car aux)) " is not equal to: "
                                                                                                     (string-append (string-trim (format "~a" (car aux)) ".in") ".out") )
                                                    #f '(ok stop))
                                       (displayln "ITS FALSE")))
                                 (let ((out (open-output-file (string-append (string-trim (format "~a" (car aux)) ".in") ".out"))))
                                   (displayln "file do not exists, creating a new one")
                                   (write (format "~a" (syntax->datum result)) out)
                                   (close-output-port out))))
                           #;(parameterize ((print-syntax-width 9000))
                               (displayln sexp))
                           #;(displayln non-expanded-program)
                           (loop)])) 
                      #t)))
                  (loop (cdr aux))))))
        (auto-tests)))
    
    ;;;;Menus definitions (lack a better idea on how to do this)
    (define detect null)
    (define clear null)
    (define AutomaticRefactoring null)
    (define RefactoringOperations null)
    (define printPythonmenu null)
    (define checkRefactoring null)
    (define wideScopeDetected #f)
    #;(define wideScope null)
    
    (define (pretty-format-improved stx)
      (define (walk-list stx)
        (displayln "not-implemented!")
        (read))
      (cond [(pair? stx) (string-append (pretty-format(syntax->datum (car stx))) (pretty-format (syntax->datum (cdr stx))))]
            [(list? stx) (walk-list stx)]
            [else (pretty-format (syntax->datum stx))]))
    (define (syntax->datum-improved stx)
      (define (walk-list stx)
        (displayln "not-implemented!")
        (read))
      
      (cond [(pair? stx) `(,@(syntax->datum (car stx)) (syntax->datum (cdr stx)))]
            [(list? stx) (walk-list stx)]
            [else (syntax->datum stx)]))
    
    (define (print-languages-syntax program expanded? text start-selection end-selection start-line end-line last-line)
      (displayln "print-languages")
      (parameterize ((print-syntax-width 9000))
        (if expanded?
            (displayln (code-walker program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line)))
            (displayln (code-walker-non-expanded program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line))))))
    
    
    
    
    (define (syntax-tests-refactoring sexp start-line)
      (void))
    (define search-refactoring #t)
    (define (syntax-refactoring program expanded? text start-selection end-selection start-line end-line last-line auto-refactoring? detect-refactorings?
                                get-refactoring-string)
      (displayln "syntax-refactoring tests")
      (displayln start-selection)
      (displayln end-selection)     
      
      (define arg null)
      (define (write-back aux-stx aux)
        (if auto-refactoring?
            ;(- (syntax-position aux) 1) (+ 3 (string-length (syntax->string aux)) (syntax-position aux))
            (unless (void? aux-stx)
              (parameterize ((print-as-expression #f)
                             (pretty-print-columns 80))
                (send text delete (- (syntax-position aux) 1) (+ 3 (string-length (syntax->string aux)) (syntax-position aux)))
                (send text insert (pretty-format-improved aux-stx) (- (syntax-position aux) 1))
                (displayln (pretty-format-improved aux-stx))))
            (unless (void? aux-stx)
              (parameterize ((print-as-expression #f)
                             (pretty-print-columns 80))
                (send text delete start-selection end-selection)
                (send text insert (pretty-format-improved aux-stx) start-selection)
                #;(displayln (pretty-format-improved aux-stx))))))
      (define (write-simple stx) ;;Geral output
        (displayln (format "~.a" (syntax->datum stx)))
        (send text delete start-selection end-selection)
        (send text insert (format "~.a" (syntax->datum stx)) start-selection)
        (displayln stx))
      
      (define (search-refactorings-highlight start)
        #;(read)
        (displayln start)
        (displayln last-line)
        (define aux (code-walker-non-expanded program start (+ 1 last-line) (+ 1 last-line)))
        #;(let*(( arg (code-walker-non-expanded program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line)))
                (racket-stx (racket-parser arg))
                (python-stx (python-parser arg))
                (processing-stx (processing-parser arg)))
            (void))
        (define arg2 (code-walker-non-expanded program start 10 10))
        (unless (and (void? (python-parser aux)) (void? (processing-parser aux)) (void? (racket-parser aux)))
          (displayln "SYNTAX FOUND")
          #;(displayln aux)
          #;(read)
          (parameterize ((print-syntax-width 9000))
            (displayln aux))
          
          (unless (and (void? (python-parser aux)) (void? (processing-parser aux)))
            (unless (void? (python-parser aux))
              (displayln (format "~.a" (syntax->datum (write-python aux))))
              #;(read)
              (send text highlight-range (- (syntax-position aux) 1)
                    (+ (string-length (format "~.a" (syntax->datum (write-python aux)))) (syntax-position aux))
                    (make-object color% 0 255 0 0.35) #:key 'key))
            (unless (void? (processing-parser aux))
              (displayln (format "~.a" (syntax->datum (write-processing aux))))
              #;(read)
              (send text highlight-range (- (syntax-position aux) 1)
                    (+ (string-length (format "~.a" (syntax->datum (write-processing aux)))) (syntax-position aux))
                    (make-object color% 0 255 0 0.35) #:key 'key))
            (displayln "in unless"))
          (if (regexp-match #rx"(\n)" (syntax->string aux))            
              (send text highlight-range (- (syntax-position aux) 1) (+ 5 (string-length (syntax->string aux)) (syntax-position aux) 
                                                                        (length(regexp-match #rx"(\n)" (syntax->string aux))))
                    (make-object color% 0 255 0 0.35) #:key 'key)
              (send text highlight-range (- (syntax-position aux) 1)
                    (+ 1 (string-length (syntax->string aux)) (syntax-position aux)) (make-object color% 0 255 0 0.35) #:key 'key)))
        (displayln "loop")
        (displayln aux)
        (+ 1 start))
      
      (define (search-refactorings program start-line end-line)
        (define (trim-string? str)
          (if (label-string? str)
              str
              (substring str 0 200)))
        (if (= start-line end-line 0)
            (unless auto-refactoring?
              (send RefactoringOperations set-label "None Available"))
            (let*(( arg (code-walker-non-expanded program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line)))
                  (racket-stx (racket-parser arg))
                  (python-stx (python-parser arg))
                  (processing-stx (processing-parser arg))
                  (meta-lang-stx (create-meta-lang arg))
                  (test-lang (meta-lang-parser arg)))
              
              
              (displayln program)
              (display "RACKET: ")
              (displayln racket-stx)
              (display "Python: ")
              (displayln python-stx)
              (display "meta-refactoring ")
              (displayln test-lang)
              (unless auto-refactoring?
                (cond [(not (void? python-stx))
                       (send RefactoringOperations set-label (trim-string? (format "~.a" (syntax->datum python-stx))))]
                      [(not (void? racket-stx))
                       (send RefactoringOperations set-label (trim-string? (format "~.a" (pretty-format (syntax->datum-improved racket-stx)))))]
                      [(not (void? processing-stx))
                       (send RefactoringOperations set-label (trim-string? (format "~.a" (syntax->datum processing-stx))))]
                      [else 
                       (send RefactoringOperations set-label "None Available")]))))
        (if (string=? (send RefactoringOperations get-label) "None Available")
            (send RefactoringOperations enable #f)
            (send RefactoringOperations enable #t)))
      
      (if get-refactoring-string
          (search-refactorings program start-line end-line)
          (if (not detect-refactorings?)
              (if expanded?
                  (syntax-parse (code-walker program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line)) ;used for the exapanded program Regarding if
                    #:literals(if)
                    [(call-with-values (lambda () (if test-expr then-expr else-expr)) print-values) 
                     (when (and (not (eval-syntax #'then-expr)) (eval-syntax #'else-expr))
                       (write-back #'(not test-expr)))])
                  ;;do the refactoring operations:
                  (let*(( arg (code-walker-non-expanded program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line)))
                        (racket-stx (racket-parser arg))
                        (python-stx (python-parser arg))
                        (processing-stx (processing-parser arg))
                        #;(arg2 (code-walker-special program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line)))
                        #;(auto-tests (racket-parser arg2)))
                    #;(displayln "special")
                    #;(displayln auto-tests)
                    (cond
                      [(not (void? racket-stx))
                       (display "RACKET: ")
                       (displayln racket-stx)
                       (write-back (racket-parser arg) arg)]
                      [(not (void? python-stx))
                       (display "Python: ")
                       (displayln python-stx)
                       (write-simple python-stx)]
                      [(not (void? processing-stx))
                       (display "Processing: ")
                       (displayln processing-stx)
                       (write-simple processing-stx)]
                      [else 
                       (send RefactoringOperations set-label "None Available")]))
                  #;(begin
                      (set! arg (code-walker-non-expanded program (+ 1 start-line) (+ 1 end-line) (+ 1 last-line)))
                      (displayln "arg")
                      (displayln arg)
                      (displayln "racket-parser")
                      (displayln (racket-parser arg))
                      (write-back (racket-parser arg) arg)))
              (begin
                (if auto-refactoring?
                    (void)
                    (if (send detect is-enabled?)
                        (begin 
                          (send detect enable #f)
                          (send clear enable #t)
                          (let loop ([start start-line]
                                     [end (+ 1 last-line)])
                            (define aux (search-refactorings-highlight start))
                            (if (> start end) 
                                (displayln "Over")
                                (loop aux end))))
                        (begin 
                          (send detect enable #t)
                          (send clear enable #f)
                          (send text unhighlight-ranges/key 'key))))))))
    
    
    (define next-trace-refresh? #t)
    (define (get-next-trace-refresh?) next-trace-refresh?)
    (define (set-next-trace-refresh b) (set! next-trace-refresh? b))
    (define current-replay-state #f)
    (define (set-replay-state rs) (set! current-replay-state #f))
    (define (get-replay-state) current-replay-state)
    
    (drracket:module-language-tools:add-online-expansion-monitor
     online-comp.rkt
     'monitor
     (λ (defs-text val)
       (define tab (send defs-text get-tab))
       #;(displayln "on monitor")
       (cond
         [(drracket:module-language-tools:start? val) (set-next-trace-refresh #t)]
         [(drracket:module-language-tools:done? val) (void)]
         [else 
          
          ;; replay-state = 
          ;;  (or/c #f                  -- no replay running
          ;;        (box #t             -- keep running this replay
          ;;             (listof (listof stuff))
          ;;                            -- pick up some new elements to add to the current replay
          ;;             #f))           -- doesn't actually get set on a tab, but this means to
          ;;                               just stop running the replay
          #;(displayln "else in cond")
          
          (when (get-next-trace-refresh?)
            (define old-replay-state (get-replay-state))
            (when (box? old-replay-state)
              (set-box! old-replay-state #f))
            (set-replay-state #f)
            (set-next-trace-refresh #f)
            
            ;; reset any previous check syntax information
            #;(send tab syncheck:clear-error-message)
            #;(send tab syncheck:clear-highlighting)
            #;(send defs-text syncheck:reset-docs-im)
            #;(send tab add-bkg-running-color 'syncheck "orchid" cs-syncheck-running)
            (send (send (send defs-text get-tab) get-frame) syncheck:init-arrows)) ;;check this
          
          (define drr-frame (send (send defs-text get-tab) get-frame))
          (cond
            [(string? val) ;; an internal error happened
             (displayln "in val")
             #;(displayln val)
             #;(send tab remove-bkg-running-color 'syncheck)
             #;(send tab show-online-internal-error val)]
            [else
             #;(displayln "else in cond in else")
             #;(displayln val)
             #;(read)
             
             ;;;;;;;;;;;;;;;
             ;;Add information to use with the arrows:
             ;;non-expanded-program and expanded-program             
             ((λ ()
                ((drracket:eval:traverse-program/multiple
                  #:gui-modules? #f
                  (send defs-text get-next-settings)
                  void
                  void)
                 (drracket:language:make-text/pos defs-text
                                                  0
                                                  (send defs-text last-position))
                 (λ (sexp loop) ;this is the "iter"
                   (cond
                     [(eof-object? sexp)
                      (void)
                      #;(custodian-shutdown-all #f)]
                     [else
                      (set! non-expanded-program sexp)
                      (loop)])) 
                 #t)))
             ;;;;;;;;;;;;;;;
             (define current-replay-state (get-replay-state))
             (cond
               [(not current-replay-state)
                (define new-replay-state (box '()))
                (set-replay-state new-replay-state)
                #;(displayln "before replay-compile-comp-trace")
                (send drr-frame replay-compile-comp-trace-aux
                      defs-text
                      val
                      (box '()))] ;; should this box be new-replay-state instead?
               [else
                (set-box! current-replay-state
                          (append (unbox current-replay-state) (list val)))])])])))
    
    (drracket:module-language-tools:add-online-expansion-handler
     online-comp.rkt
     'go
     void)
    
    (define (phase1) 
      (void))
    (define (phase2) (void))
    (drracket:get/extend:extend-unit-frame refactoring-tool-mixin)))
(define-runtime-path online-comp.rkt "online-comp.rkt")
(define cs-syncheck-running "Check Syntax Running") ;;before 6.3
#;(define cs-syncheck-running (string-constant cs-syncheck-running)) ;;;for 6.3