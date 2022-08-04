#lang racket

(define (any? v) #t)

(define (debug . args)
   (for ([arg args]) (display arg) (display " "))
   (newline))

(define (id x) x)

(define (init x)
  (if (null? x) '() 
    (reverse (cdr (reverse x)))))

(define (last x)
  (if (null? x) (error "no last in list" x)
    (car (reverse x))))

(begin-for-syntax
   (struct syntax-location (line column) #:transparent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Actor APIs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An actor is represented by its thread
(struct actor (thd))

(define (create behavior . arguments)
  (let ((initial-behavior (apply behavior arguments)))
    (actor (thread (lambda ()
              (let loop ((behavior initial-behavior))
                (let* ((msg (thread-receive))
                       (tag (message-tag msg))
                       (arguments (message-arguments msg)))

                  (loop (if (contracted-message? msg) 
                      (handle-contracted behavior (contracted-message-contract msg) tag arguments)
                      (apply behavior tag arguments))))))))))

(define (become new-behavior . args)
  (apply new-behavior args))

(define (terminate) 
  (kill-thread (current-thread)))

(define (wait-until-termination act)
  (thread-wait (actor-thd act)))

;; regular actor send, specialisation of `send` that also works on contracted actors
(define-syntax send/a
  (syntax-rules ()
    ((send/a receiver tag arg ...)
     (actor-send receiver (message (quote tag) (list arg ...))))))

;; Internal functions
(define (actor-send actor datum) 
  (thread-send (actor-thd actor) datum))

(define (actor-wait actor)
  (thread-wait (actor-thd actor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Contract handling during message handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; thread-local variable that is set to ensure that certain calls are made during the 
;; dynamic extent of handling a message.
(define $ensure-sends$ (make-parameter #f))

;; checks whether we are in a context where we are ensuring that a particular set of 
;; `send` statements occur.
;; () -> bool
(define (ensuring-send?)
   ($ensure-sends$))

;; marks a specific message to say that is has been sent 
(define (mark-message-as-sent tag arguments positions)
  (debug "receiving positions" positions)
  (if (and (ensuring-send?) (hash-has-key? ($ensure-sends$) tag))
    (let ((contracts (car (hash-ref ($ensure-sends$) tag))))
      (debug "contracts checking " contracts "for" arguments)
      (for-each (lambda (c argument pos) (contract c argument pos 'neg))
                contracts arguments positions)
       ($ensure-sends$ (hash-update ($ensure-sends$) tag (lambda (contract-and-flag) (cons (car contract-and-flag) #t)))))
    ; do nothing if not in an ensure send context
    '()))

;; checks whether all the messages thart are in the current $ensure-sends$ are marked
(define (all-sent?) 
  (let ((ensure-sends ($ensure-sends$)))
    (andmap id (hash-values ensure-sends))))


(define (handle-contracted behavior contract tag arguments)
  ;; todo: blame the actor if the tag is not available in its contract
  (define expected-message-tags (map m->-tag (message/c-ensure-sends contract)))
  (define expected-message-arguments (map m->-contracts (message/c-ensure-sends contract)))
  (debug "got expected tags" expected-message-tags)
  (define ensure-sends (make-immutable-hash (map (lambda (tag contract) (cons tag (cons contract #f))) expected-message-tags expected-message-arguments)))
  (parameterize ([$ensure-sends$ ensure-sends])
     (let ((result (apply behavior tag arguments)))
       (if (all-sent?)
           result
           ;; todo: produce a blame, blaming the correct parties 
           (error "actor did not sent all messages it promised to send")))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ask Pattern API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax ask 
  (syntax-rules ()
    ((ask receiver tag param ...) 
     (let*
       ((res (mcons 'response '()))
        (reply-to (create (behavior "reply-to" () (messages ((reply (v) (set-mcdr! res v) (terminate))))))))

       (send receiver tag reply-to param ...)
       ;; representation of a "future", together with await they form a compatible API for what 
       ;; is implemented in MAF
       (lambda ()
          (actor-wait reply-to)
          (mcdr res))))))

(define (await fut)
  (fut))

(define-syntax reply 
  (syntax-rules ()
    ((reply to v) 
     (begin 
        (displayln (format "replying to ~a with ~a" to v))
        (send to reply v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Behaviors 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct behavior/s (constructor)
        #:property prop:procedure (struct-field-index constructor)
        #:reflection-name 'behavior)

(define-syntax (behavior stx)
  (syntax-case stx ()
    ((behavior name ((param param-contract) ...)
       handler)
     #'(behavior/s (lambda (param ...)
       ;; return a receival lambda
       (lambda (msg-tag . arguments)
         (apply handler (cons msg-tag arguments))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Messages (meta-level)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a regular message, with no contracts attached 
(struct message (tag arguments))

;; a contracted message
(struct contracted-message message (contract))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Message handlers syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (contract-inserter stx) 
  (let ((datum (syntax->datum stx)))
    (datum->syntax stx `(list ,@(for/list [(i (in-range 0 (- (length datum) 1)))] 'any?)))))

(define-syntax messages 
  (syntax-rules ()
    ((messages ((tag (param ...) body ...) ...))
     (lambda (msg-tag . arguments)
       ;; try the message handlers in order, until one does not produce 'invalid
       (define (loop handler) 
         (if (null? handler)
           (error (format "no suitable handler for ~a found" msg-tag))
            (let ((res (apply (car handler) (cons msg-tag arguments))))
              (if (eq? res 'invalid)
                (loop (cdr handler))
                res))))
       (loop (list (message/s (quote tag) (contract-inserter #'(param ...)) '() '() (lambda (param ...) body ...)) ...))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Message Handlers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct message/s (tag arguments ensure-send ensure-becomes handler)
        #:property prop:procedure (lambda (msg msg-tag . args) 
            (if (eq? msg-tag (message/s-tag msg))
                ;; todo also check sends and becomes after application
                (begin 
                  (for-each (lambda (arg c) 
                         (contract c arg arg msg))
                       args
                       (message/s-arguments msg))
                  (apply (message/s-handler msg) args))
                'invalid)))

; (define (message tag #:arguments args #:ensure-send sends #:ensure-becomes becomes #:handler hndl)
;   (message/s args sends becomes hndl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Contracts on the behavior of the actor 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct actor-mon (actor contract))

;; a contract on the behavior of an actor 
;; 
;; @param constructor-contract a list of contracts on the constructor parameters of the behavior
;; @param message-list a hashtable of mapping message tags to their contract
(struct behavior/contract 
   (constructor-contract message-list name)
   #:methods gen:custom-write [(define (write-proc contract port mode) (fprintf port "~a" (behavior/contract-name contract)))]
   ; behavior like a flat contract
   #:property prop:procedure 
      (lambda (contract actor) 
        (and (actor-mon? actor) 
             (eq? (actor-mon-contract actor) contract))))

;; a contract on the handling of a message.
;; properties of interest are: existance of message handler .
;; ensuring that particular message sends happen.
;; ensuring that the actor ends up in a particular behavior.
(struct message/c (parameter-contracts expected-tag ensure-sends ensure-behaviors))

;; Syntax for contract creation over the behavior of an actor
(define-syntax behavior/c 
  (syntax-rules (ensure-send)
     ((behavior/c name (constructor-contract ...) 
                 ((tag (param-contract ...) (ensure-send send ...) (ensure-becomes become ...)) ...))
      (behavior/contract (list constructor-contract ...) 
         (make-hash
            (list (cons (quote tag) (message/c (list param-contract ...) (quote tag) (list send ...) (list become ...))) ...)) name))
     ((behavior/c name (constructor-contract ...)
                  ((tag (param-contract ...) (ensure-send send ...)) ...))
                  (behavior/c name (constructor-contract ...) 
                     ((tag (param-contract ...) (ensure-send send ...) (ensure-becomes)) ...)))
     ((behavior/c name (constructor-contract ...) 
                  ((tag (param-contract ...)) ...))
      (behavior/c name (constructor-contract ...)
                  ((tag (param-contract ...) (ensure-send) (ensure-becomes)) ...)))))

;; create a contracted actor reference
(define-syntax (create/c stx) 
  (syntax-case stx () 
    ((create/c beh/c behavior args ...)
     (let ((positions (map (lambda (stx) (syntax-location (syntax-line stx) (syntax-column stx))) (syntax->list #'(args ...)))))
        #`(let* ((args$ (list args ...))
                (actor (apply create behavior args$))
                (beh/c$ beh/c))
           ;; check whether the constructors hold for the arguments 
           (for-each (lambda (argument c pos) 
            (contract c argument pos 'neg))
            args$
            (behavior/contract-constructor-contract beh/c$)
            (list #,@positions))
           (actor-mon actor beh/c$))))))

;; a contract on a message send 
(struct m-> (tag contracts))

(define-syntax ->m
  (syntax-rules ()
    ((->m tag contracts ...) (m-> (quote tag) (list contracts ...)))))


;; A send on a contracted entity, is used internally by `send` when it can dispatch to a contracted send.
(define-syntax (send/c stx)
  (syntax-case stx () 
    ((send/c target tag arguments ...) 
     (let*
       ((args (syntax->list #'(arguments ...)))
        (positions (map (lambda (arg) (syntax-location (syntax-line arg) (syntax-column arg))) args)))

     #`(let*
       ((target-contract (actor-mon-contract target))
        (message-contract (hash-ref (behavior/contract-message-list target-contract) (quote tag))))
       ;; check the arguments of the contract
       (for-each (lambda (c argument position)
                   (contract c argument position 'neg))
                 (message/c-parameter-contracts message-contract)
                 (list arguments ...)
                 (list #,@positions))
       ;; actually send the message but with the message/c contract attached 
       (actor-send (actor-mon-actor target) (contracted-message (quote tag) (list arguments ...) message-contract)))))))

(define-syntax (send stx)
  (syntax-case stx ()
    ((send target tag arguments ...) 
     (let ((positions (map (lambda (arg) (syntax-location (syntax-line arg) (syntax-column arg))) (syntax->list #'(arguments ...)))))
        #`(begin (mark-message-as-sent (quote tag) (list arguments ...) (list #,@positions))
               (ensure-protocol (quote tag) (list arguments ...))
               (cond 
                 ((actor? target)  (send/a target tag arguments ...))
                 ((actor-mon? target) (send/c target tag arguments ...))
                 (else (error (format "expected an actor or contracted actor, ~a is neither." target)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Message protocol contracts 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax protocol/c 
  (syntax-rules ()
    ((protocol/c name 
      (start start-state)
      (from-state (on message to-state) ...) ...)
     (protocol (quote start-state) 
               (make-hash (list (cons (quote from-state) (make-hash (list (cons (quote message) (quote to-state)) ...))) ...))))))

(struct protocol (start states))

(define (protocol-successors protocol tag)
  (hash-ref (protocol-states protocol) tag))

(define $protocol$ (make-parameter #f))

(struct protocol-state (tag protocol))
(define (has-next? current next-message)
  (let*
    ((current-tag (protocol-state-tag current))
     (protocol (protocol-state-protocol current))
     (current-successors (protocol-successors protocol current-tag)))
    (member next-message (hash-keys current-successors))))

;; Produces a successor state for the given tag, if the successor state is invalid produces #f
(define (go-next current next-message)
  (let*
    ((current-tag (protocol-state-tag current))
     (protocol (protocol-state-protocol current))
     (current-successors (protocol-successors protocol current-tag)))
    (if (has-next? current next-message)
        (let ((next-tag (hash-ref current-successors next-message)))
          (protocol-state next-tag protocol))
        #f)))
        
(define (is-final? current)
  (let*
    ((current-tag (protocol-state-tag current))
     (protocol (protocol-state-protocol current))
     (current-successors (protocol-successors protocol current-tag)))
    (null? (hash-values current-successors))))
  
;; mutates the thread-local field $protocol$ to go to the next tag
(define (go-next! next-tag)
  (let ((current ($protocol$)))
    ($protocol$ (go-next current next-tag))))


(struct ensure-protocol/f-wrapper (protocol f)
   #:property prop:procedure
      (lambda (self . args) 
        (let ((protocol (ensure-protocol/f-wrapper-protocol self))
              (f (ensure-protocol/f-wrapper-f self)))
        (parameterize [($protocol$ (protocol-state (protocol-start protocol) protocol))]
          ;; run the function 
          (let ((res (apply f args)))
             ;; then check whether we are in a final state 
             (if (is-final? ($protocol$))
                 ;; simply return the result 
                 res 
                 ;; otherwise create a blame error (todo)
                 (error "protocol not finished till completion currently in state" (protocol-state-tag ($protocol$)))))))))

;; ensure that the given procedure behaves like the given protocol when applied
(struct ensure-protocol/f (protocol) 
   #:property prop:contract 
      (build-contract-property 
        #:projection (lambda (self) (lambda (blm) (lambda (f)
         (ensure-protocol/f-wrapper (ensure-protocol/f-protocol self) f))))))

;; called by `send` to track position in the protocol
(define (ensure-protocol tag arguments)
  (if (and ($protocol$) (has-next? ($protocol$) tag))
      (go-next! tag)
      ;; todo, generate blame error
      (when ($protocol$)
          (error "not in the correct position in the protocol " (protocol-state-tag ($protocol$))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Demo code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; behavior contracts demo code

(define player 
  (behavior "player" ((team-id any?))
    (messages ((get-team (reply-to) (reply reply-to 42))))))

(define player/c (behavior/c "player/c"
                   (integer?)
                   ((get-team (actor?) (ensure-send (->m reply integer?)))))) 

(define/contract (run-application pl)
  (-> player/c integer?)
  (await (ask pl get-team)))

(define myplayer (create/c player/c player 1))
(run-application myplayer)

;; messsage protocol contracts (see Christophe Scholliers PhD thesis) (on the sender side)
;; a buyer must first login, then buy something and finally logout.
;; it is also allowed to logout without buying anything
(define buyer/c
  (protocol/c "buyer/c"
    (start init)
    (init 
      (on login next))
    (next 
      (on buy next)
      (on logout end))
    (end)))

(define (buyer-impl seller)
  (send seller login)
  (send seller buy)
  (send seller logout))

(define/contract (test-buyer buyer)
   (-> (ensure-protocol/f buyer/c) any?)
   (let ((seller (create (behavior "seller" () (messages ((login () '()) (buy () '()) (logout () '())))))))
      (buyer seller)))

(test-buyer buyer-impl)
