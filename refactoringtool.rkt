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
         ;racket/string
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
                                    #:detect-refactorings [detect-refactorings? #f] #:get-refactoring-string [get-refactoring-string #f] #:check-refactorings [check-refactorings? #f])
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
          
          
          (define (check-similar start-line)
            (displayln "check-similar")
            (define (search-similiar sexp randombool text start-selection end-selection 
                                     start-line end-line last-line check-refactorings?)
              
              (define (compare-syntax userSelected autoSelected)
                ;;; add a boolean that comapres values or not
                ;;; end cases, a #f or userStacl and autoStack both reach null at the same time
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
              (define selected-expressions (code-walker-non-expanded program (+ 1 start-line) (+ 1 end-line)))
              ;;;create "rule" get essentials of the "form" (is the "best" way)
              ;;; or dumb way, compare for exact matches
              (define (search-refactorings start)
                (displayln start)
                (displayln last-line)
                (define aux (code-walker-non-expanded program (+ 1 start) (+ 1 last-line)))
                #;(unless (void? (racket-parser selected-expressions))
                    (send text highlight-range (syntax-position selected-expressions) 
                          (+ 2 (string-length (syntax->string aux)) (syntax-position selected-expressions)) (make-object color% 0 255 0 0.35) #:key 'key))
                ;;; aux is the form to compare with
                ;;; selected-expression is the form have
                (define result (compare-syntax selected-expressions aux))
                (when result
                  ;;; there is a match
                  (displayln "match found!!")
                  (send text highlight-range (syntax-position selected-expressions) 
                        (+ 2 (string-length (syntax->string aux)) (syntax-position selected-expressions)) (make-object color% 0 255 0 0.35) #:key 'key)
                  (send text highlight-range (syntax-position aux) 
                        (+ 2 (string-length (syntax->string aux)) (syntax-position aux)) (make-object color% 0 200 0 0.25) #:key 'key))
                #;(send text unhighlight-range start end color [caret-space style])
                ;;;;; highlight/display (end start color)
                #;(send text highlight-range 1 5 (make-object color% 255 0 0 1.0))
                
                (displayln "loop")
                (displayln aux)
                (if (= (+ 1 start) last-line)
                    (displayln "Over")
                    (search-refactorings (+ 1 start))))
              
              (search-refactorings start-line))
            (define (search-similar-aux)
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
          
          (define (automated-refactoring start-line)
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
                (drracket:language:make-text/pos text
                                                 0
                                                 (send text last-position))
                (λ (sexp loop) ;this is the "iter"
                  (cond
                    [(eof-object? sexp)
                     (custodian-shutdown-all user-custodian)]
                    [else
                     (displayln sexp)
                     (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line last-line last-line auto-refactoring? detect-refactorings? get-refactoring-string)
                     (parameterize ((print-syntax-width 9000))
                       (displayln sexp))
                     (displayln not-expanded-program)
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
                         (displayln not-expanded-program)
                         (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line end-line last-line auto-refactoring? detect-refactorings? get-refactoring-string)
                         
                         (loop)])) 
                    #t)))))
          (send text begin-edit-sequence)
          (send text insert " " (send text last-position))
          (send text delete (- (send text last-position) 1) (send text last-position))
          (send text end-edit-sequence)
          (when get-refactoring-string
            (let ((result "testeBro" )
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
                       (displayln (syntax-refactoring sexp #f (get-definitions-text) start-selection end-selection start-line end-line last-line auto-refactoring? detect-refactorings? get-refactoring-string))
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
        (make-object menu-item%
          ;(get-refactoring-string)
          "Check Refactoring"
          refactoring-menu
          (λ (item evt)
            (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:detect-refactorings #f #:check-refactorings #t)))
        
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
        
        (define (create-refactoring-menu menu bool)
          (define refactoring-menu
            (make-object menu%
              "Refactoring Test Plugin"
              menu))
          (set! RefactoringOperations 
                (make-object menu-item%
                  "Refactoring operations"
                  refactoring-menu
                  (λ (item evt)
                    (refactoring-syntax (get-current-tab) (get-interactions-text) #f))))
          (displayln "$$$$$$$$$$$$$$$$$$$$$$$$$$ LABEL")
          (displayln (send RefactoringOperations get-label))
          (refactoring-syntax (get-current-tab) (get-interactions-text) #f #:get-refactoring-string #t)
          (displayln "@@@@@@")
          (void))
        (keymap:add-to-right-button-menu/before
         (let ([old (keymap:add-to-right-button-menu/before)])
           (λ (menu editor event)
             (old menu editor event)
             (create-refactoring-menu menu #f))))))
    
    ;;;;Menus definitions (lack a better idea on how to do this)
    (define detect null)
    (define clear null)
    (define AutomaticRefactoring null)
    (define RefactoringOperations null)
    
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
      
      (define (search-refactorings-highlight start)
        (displayln start)
        (displayln last-line)
        (define aux (code-walker-non-expanded program (+ 1 start) (+ 1 last-line)))
        (unless (void? (racket-parser aux))
          (if (regexp-match #rx"(\n)" (syntax->string aux))            
              (send text highlight-range (- (syntax-position aux) 1) (+ 1 (string-length (syntax->string aux)) (syntax-position aux) 
                                                                        (length(regexp-match #rx"(\n)" (syntax->string aux))))
                    (make-object color% 0 255 0 0.35) #:key 'key)
              (send text highlight-range (- (syntax-position aux) 1)
                    (+ 1 (string-length (syntax->string aux)) (syntax-position aux)) (make-object color% 0 255 0 0.35) #:key 'key)))
        (displayln "loop")
        (displayln aux)
        (+ 1 start))
      (define (search-refactorings program start-line end-line)
        (if (= start-line end-line)
            (send RefactoringOperations set-label "None Available")
            (let*(( arg (code-walker-non-expanded program (+ 1 start-line) (+ 1 end-line)))
                  (racket-stx (racket-parser arg))
                  (python-stx (python-parser arg)))
              (display "RACKET: ")
              (displayln racket-stx)
              (display "Python: ")
              (displayln python-stx)
              (if (void? python-stx)
                  (if (void? racket-stx)
                      (send RefactoringOperations set-label "None Available")
                      (send RefactoringOperations set-label (pretty-format (syntax->datum racket-stx))))
                  (send RefactoringOperations set-label "PYTHON - Refactoring"))))
        (if (string=? (send RefactoringOperations get-label) "None Available")
            (send RefactoringOperations enable #f)
            (send RefactoringOperations enable #t)))
      (if get-refactoring-string
          (search-refactorings program start-line end-line)
          (if (not detect-refactorings?)
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
    (define (phase1) 
      (void))
    (define (phase2) (void))
    (drracket:get/extend:extend-unit-frame refactoring-tool-mixin)))