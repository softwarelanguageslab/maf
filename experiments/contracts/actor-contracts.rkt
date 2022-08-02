#lang racket

(define (any? v) #t)

(define (debug . args)
   (for ([arg args]) (display arg) (display " "))
   (newline))


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
                  (loop (apply behavior tag arguments)))))))))

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

(define (reply to v) 
  (displayln (format "replying to ~a with ~a" to v))
  (send to reply v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Behaviors 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct behavior/s (constructor)
        #:property prop:procedure (struct-field-index constructor)
        #:reflection-name 'behavior)

(define-syntax behavior 
  (syntax-rules ()
    ((behavior name ((param param-contract) ...)
       handler)
     (behavior/s (lambda (param ...)
       ;; check the contract on the contructor arguments
       (contract param-contract param 'pos 'neg) ... 
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
   (constructor-contract message-list))

;; a contract on the handling of a message.
;; properties of interest are: existance of message handler .
;; ensuring that particular message sends happen.
;; ensuring that the actor ends up in a particular behavior.
(struct message/c (parameter-contracts expected-tag ensure-sends ensure-behaviors))

;; Syntax for contract creation over the behavior of an actor
(define-syntax behavior/c 
  (syntax-rules (ensure-send)
     ((behavior/c (constructor-contract ...) 
                 ((tag (param-contract ...) (ensure-send send ...) (ensure-becomes become ...)) ...))
      (behavior/contract (list constructor-contract ...) 
         (make-hash
            (list (cons (quote tag) (message/c (list param-contract ...) (quote tag) (list send ...) (list become ...))) ...))))
     ((behavior/c (constructor-contract ...)
                  ((tag (param-contract ...) (ensure-send send ...)) ...))
                  (behavior/c (constructor-contract ...) 
                     ((tag (param-contract ...) (ensure-send send ...) (ensure-becomes)) ...)))
     ((behavior/c (constructor-contract ...) 
                  ((tag (param-contract ...)) ...))
      (behavior/c (constructor-contract ...)
                  ((tag (param-contract ...) (ensure-send) (ensure-becomes)) ...)))))

;; create a contracted actor reference
(define (create/c beh/c behavior . args)
   (let ((actor (apply create behavior args)))
     ;; check whether the constructors hold for the arguments 
     (for-each (lambda (argument c) 
      (contract c argument 'pos 'neg))
      args
      (behavior/contract-constructor-contract beh/c))
     (actor-mon actor beh/c)))

;; A send on a contracted entity, is used internally by `send` when it can dispatch to a contracted send.
(define-syntax send/c 
  (syntax-rules () 
    ((send/c target tag arguments ...) 
     (let*
       ((target-contract (actor-mon-contract target))
        (message-contract (hash-ref (behavior/contract-message-list target-contract) (quote tag))))
       ;; check the arguments of the contract
       (for-each (lambda (c argument)
                   (contract c argument 'pos 'neg))
                 (message/c-parameter-contracts message-contract)
                 (list arguments ...))
       ;; actually send the message but with the message/c contract attached 
       (actor-send (actor-mon-actor target) (contracted-message (quote tag) (list arguments ...) message-contract))))))

(define-syntax send 
  (syntax-rules ()
    ((send target tag arguments ...) 
     (cond 
       ((actor? target)  (send/a target tag arguments ...))
       ((actor-mon? target) (send/c target tag arguments ...))
       (else (error (format "expected an actor or contracted actor, ~a is neither." target)))))))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Demo code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define player 
  (behavior "player" ((team-id any?))
    (messages ((get-team (reply-to) (reply reply-to team-id))))))

(define player/c (behavior/c 
                   (integer?)
                   ((get-team (actor?)))))

(define myplayer (create/c player/c player 1))
(define answer (await (ask myplayer get-team)))
(displayln answer)
