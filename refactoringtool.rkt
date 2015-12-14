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
         (for-template racket/base) ;was a test, did not work
         syntax/to-string
         "languageRefactorings/racket-refactorings.rkt"
         "languageRefactorings/python-refactorings.rkt"
         "code-walker.rkt") 
(provide tool@)
(define not-expanded-program null)
(define expanded-program null)
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
                 )
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
        
        (define current-syncheck-running-mode #f)
        
        (define (refactoring-syntax tab interactions refactoring? #:print-extra-info? [print-extra-info? #f] #:auto-refactoring [auto-refactoring? #f] 
                                    #:detect-refactorings [detect-refactorings? #f])
          (define definitions-text (get-definitions-text))
          (define text (get-definitions-text))
          (define interactions-text interactions)
          (define drs-eventspace (current-eventspace))
          (define the-tab (get-current-tab))
          (define-values (old-break-thread old-custodian) (send the-tab get-breakables))
          
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
          
          ;; set by the init-proc
          (define expanded-expression void)
          (define expansion-completed void)
          (define user-custodian #f)
          
          (define normal-termination? #f)
          
          (define show-error-report/tab
            (void)
            #;(λ () ; =drs=
                #;(send the-tab turn-on-error-report)
                #;(send (send the-tab get-error-report-text) scroll-to-position 0)
                #;(when (eq? (get-current-tab) the-tab)
                    (show-error-report))))
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
                                           print-extra-info?)) ;; set by set-directory above
              (set! user-custodian (current-custodian))))
          (define settings (send definitions-text get-next-settings))
          (displayln "expand")
          (define module-language?
            (is-a? (drracket:language-configuration:language-settings-language settings)
                   drracket:module-language:module-language<%>))
          
          (define (automated-refactoring start-line)
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
                     (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line last-line last-line auto-refactoring? detect-refactorings?)
                     (parameterize ((print-syntax-width 9000))
                       (displayln sexp))
                     (displayln not-expanded-program)
                     (loop)])) 
                #t)))
            (unless (>= start-line last-line)
              (automated-refactoring (+ 1 start-line))))
          
          (send text begin-edit-sequence)
          (send text insert " " (send text last-position))
          (send text delete (- (send text last-position) 1) (send text last-position))
          (when auto-refactoring?
            (automated-refactoring 0))
          (unless auto-refactoring?
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
                         (displayln not-expanded-program)
                         (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line end-line last-line auto-refactoring? detect-refactorings?)
                         
                         (loop)])) 
                    #t)))))
          (send text end-edit-sequence))
        (define refactoring-menu  (new menu% [label "Refactoring Menu"] [parent (send (send (send (get-definitions-text) get-tab) get-frame) get-menu-bar)]))
        (append-editor-operation-menu-items refactoring-menu #t)
        (when search-refactoring
          (make-object menu-item%
            ;(get-refactoring-string)
            "Detect/Clear Refactorings"
            refactoring-menu
            (λ (item evt)
              (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:detect-refactorings #t))))
        (make-object menu-item%
          ;(get-refactoring-string)
          "Automatic Refactoring"
          refactoring-menu
          (λ (item evt)
            (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:auto-refactoring #t)))
        
        (define (create-refactoring-menu menu bool)
          (define refactoring-menu
            (make-object menu%
              "Refactoring Test Plugin"
              menu))
          (make-object menu-item%
            "Refactoring operations"
            refactoring-menu
            (λ (item evt)
              (refactoring-syntax (get-current-tab) (get-interactions-text) #f)))
          (void))
        (keymap:add-to-right-button-menu/before
         (let ([old (keymap:add-to-right-button-menu/before)])
           (λ (menu editor event)
             (old menu editor event)
             (create-refactoring-menu menu #f))))))
    (define search-refactoring #t)
    (define (syntax-refactoring program expanded? text start-selection end-selection start-line end-line last-line auto-refactoring? detect-refactorings?)
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
                (send text insert (pretty-format (syntax->datum aux-stx)) (- (syntax-position aux) 1))
                (displayln (pretty-format (syntax->datum aux-stx)))))
            (unless (void? aux-stx)
              (parameterize ((print-as-expression #f)
                               (pretty-print-columns 80))
                  (send text delete start-selection end-selection)
                  (send text insert (pretty-format (syntax->datum aux-stx)) start-selection)
                  #;(displayln (pretty-format (syntax->datum aux-stx)))))))
      (define (write-python stx)
        (send text delete start-selection end-selection)
        (send text insert stx start-selection 'same)
        (displayln stx))
      
      (define (search-refactorings start)
        (displayln start)
        (displayln last-line)
        (define aux (code-walker-non-expanded program (+ 1 start) (+ 1 last-line)))
        #;(syntax-parse aux
            ;#:literals ((if if #:phase 2))
            ;#:literals (if)
            ;#:literal-sets (xpto)
            #:datum-literals (if) ;This works
            [(if test-expr then-expr else-expr) 
             (when (and (not (syntax->datum #'then-expr)) (syntax->datum #'else-expr))
               (send text highlight-range (syntax-position aux) (+ 2 (string-length (syntax->string aux)) (syntax-position aux)) (make-object color% 255 0 0 0.35) #:key 'key))]
            [_ 'ok])
        (unless (void? (racket-parser aux))
          (send text highlight-range (syntax-position aux) (+ 2 (string-length (syntax->string aux)) (syntax-position aux)) (make-object color% 0 255 0 0.35) #:key 'key))
        
        #;(send text unhighlight-range start end color [caret-space style])
        ;;;;; highlight/display (end start color)
        #;(send text highlight-range 1 5 (make-object color% 255 0 0 1.0))
        
        (displayln "loop")
        (displayln aux)
        (+ 1 start))
      (if (not detect-refactorings?) #;(not (= start-selection end-selection))
          (if expanded?
              (syntax-parse (code-walker program (+ 1 start-line) (+ 1 end-line)) ;used for the exapanded program Regarding if
                #:literals(if)
                [(call-with-values (lambda () (if test-expr then-expr else-expr)) print-values) 
                 (when (and (not (eval-syntax #'then-expr)) (eval-syntax #'else-expr))
                   (write-back #'(not test-expr)))])
              (begin
                ;;Regarding Refactoring if V2
                (set! arg (code-walker-non-expanded program (+ 1 start-line) (+ 1 end-line)))
                (displayln "arg")
                (displayln arg)
                (displayln "racket-parser")
                (displayln (racket-parser arg))
                (write-back (racket-parser arg) arg)
                #;(syntax-parse arg
                    ;#:literals ((if if #:phase 2))
                    ;#:literals (if)
                    ;#:literal-sets (xpto)
                    #:datum-literals (if :False :True expr-stmt) ;This works
                    [(if test-expr then-expr else-expr) 
                     (when (and (not (syntax->datum #'then-expr)) (syntax->datum #'else-expr))
                       (write-back #'(not test-expr)))]
                    [(cond ((py-truth (py-lt arg arg2)) (expr-stmt :False)) (else (expr-stmt :True))) 
                     (write-python (string-append "not(" (syntax->string #'(arg)) "<" (syntax->string #'(arg2)) ")" ))])))
          (begin
            (if auto-refactoring?
                (void)
                (if search-refactoring
                    (begin 
                      (set! search-refactoring #f)
                      (let loop ([start start-line]
                                 [end last-line])
                        (define aux (search-refactorings start))
                        (if (= start end)
                            (displayln "Over")
                            (loop aux end))))
                    (begin 
                      (set! search-refactoring #t)
                      (send text unhighlight-ranges/key 'key))))))) 
    (define (phase1) 
      (void))
    (define (phase2) (void))
    (drracket:get/extend:extend-unit-frame refactoring-tool-mixin)))