#lang racket

(provide 
  (rename-out [actor actor-struct])
  actor?
  actor-tid
  create 
  send 
  create-with-mirror
  base/fail
  become 
  envelope 
  envelope-receiver
  envelope-message
  message
  message-tag 
  message-arguments
  terminate
  wait-until-termination
  base/send-envelope
  base/create-with-mirror
  base/create
  base/reply
  base/send
  mirror
  mirror! 
  ask 
  (rename-out [reply reply-to])
  (rename-out [self/m self])
  reply
  ;; create-behavior and make-handlers are meta-functions that can be used on a base-level
  ;; to create behavior and handler values without using the actor syntax.
  ;; TODO: behavior does not actually have the correct signature for the meta code in session.rtk 
  ;; this should be changed so that self and the sender are also included.
  ;; for this the semantics should be changed such that each message (or its envelope) also includes the sender.
  (rename-out [behavior create-behavior])
  make-handlers
  ;; Syntax for defining behaviors.
  (rename-out [behavior/macro actor]))

#|
This module defines an actor system embedded into Racket.
The actor system provides meta-programming faculities for intercession and introspection using mirrors. 
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actor API 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (reply actor v)
   (send actor answer v))

(define-syntax create 
  (syntax-rules ()
    ((create behavior argument ...)
     (actor-create #f behavior argument ...))))

(define-syntax send 
  (syntax-rules ()
    ((send actor tag argument ...)
     (actor-send actor (quote tag) (list argument ...)))))

(define-syntax become 
  (syntax-rules ()
    ((become behavior argument ...)
     (behavior argument ...))))

(define-syntax self/m
  (syntax-id-rules ()
    [self/m (self)]))

(define-syntax behavior/macro
  (syntax-rules ()
    ((behavior/macro name (constructor-parameter ...) (tag (parameter ...) body ...) ...)
     ;#:with self-stx (datum->syntax #'name 'self)
     (lambda (constructor-parameter ...)
       (behavior name 
         (make-hash (list (cons (quote tag) (lambda (parameter ...) 
                                                body ...)) ...)))))
    ((behavior/macro (constructor-parameter ...) (tag (parameter ...) body ...) ...)
     (behavior 'none (constructor-parameter ...) (tag (parameter ...) body ...) ...))))

(define-syntax ask 
  (syntax-rules ()
    ((ask actor tag argument ...)
     ;; define an empheral actor which will be used as the recipient of the message
     (let* ((answer-cell (mcons 'error 'answer))
            (reply-to (create (behavior/macro 'empheral () 
                                    (answer (v) (set-mcdr! answer-cell v) (terminate))
                                    (fail (m) (set-mcar! answer-cell m) (terminate))))))

       (send actor tag reply-to argument ...)
       (wait-until-termination reply-to)
       (if (eq? (mcar answer-cell) 'error)
           (mcdr answer-cell)
           (error (mcar answer-cell)))))))
           

;; Blocks the current thread until the given actor has terminated
(define (wait-until-termination actor)
  (ensure actor? actor "should be an actor")
  (thread-wait (actor-tid actor)))


(define (terminate) 
  (actor-terminate (self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging  for soundness tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (log/create actor behavior arguments) 'todo)
(define (log/send actor tag arguments) 'todo)
(define (log/become actor behavior arguments) 'todo)
(define (log/terminate actor) 'todo)
(define (log/receive actor message) 'todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions/macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ensure pred vlu msg)
  (unless (pred vlu)
    (error (format "~a failed to satisfy ~a" vlu msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions/macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Messages
(struct message (tag arguments) #:transparent)

;; Envelopes
(struct envelope (receiver message) #:transparent)

;; An actor is described by its thread 
(struct actor (tid) #:transparent)

;; Low-level function for sending messages to the given actor
(define (actor-send-message actor m) 
  (ensure actor? actor "should be actor")
  (let ((tid (actor-tid actor)))
    (thread-send tid m)))

;; Send a message from the current actor to another actor
(define (actor-send actor tag arguments)
  (log/send actor tag arguments)
  (intercept 
    (actor-send-message actor (message tag arguments))
    ;; for the meta-level we send the message to the mirror ; the mirror is then responsible for sending the message (if needed)
    (ask (get-mirror) send (envelope actor (message tag arguments)))))
  
;; Low-level function for creating an actor
(define (actor-create mirror behavior-constructor . arguments)
  (ensure procedure? behavior "should be procedure that evaluates to a behavior")
  (intercept 
     (let* 
       ((tid (thread (lambda ()
          (let ((self (thread-receive))
                (initial-behavior (apply behavior-constructor arguments)))
            (log/create self initial-behavior arguments)
            (install-state! self mirror)
            (behavior-handler initial-behavior)))))
        (new-actor (actor tid)))

       (thread-send tid new-actor)
       new-actor)
     ;; meta-level 
     (ask (get-mirror) create behavior-constructor arguments)))

;; Terminate the given actor
(define (actor-terminate actor)
  (ensure actor? actor "should be an actor")
  (kill-thread (actor-tid actor)))

;; A function that constructs handlers for use in an instance of the `behavior` struct
(define (make-handlers association-list)
  (make-hash association-list))

;; A structure defining a behavior. 
;; It consists of an optional name and some handlers
(struct behavior (name handlers))

;; Look up the handler in the given behavior for the given tag
(define (lookup-handler behavior tag) 
  (let ((vlu (hash-ref (behavior-handlers behavior) tag #f)))
    (if vlu vlu (error (format "handler for ~a not found" tag)))))

;; Message receive loop. Should be executed in the thread of the actor
(define (behavior-handler beh)
  (ensure behavior? beh "should be a behavior")
  (let ((msg (thread-receive)))
    (let ((status (call-with-current-continuation 
                                       (lambda (cc) 
                                         (install-fail-continuation! cc)
                                         cc))))
    (if (eq? status 'fail)
        (behavior-handler beh)
        (behavior-handler (behavior-handle-message beh msg))))))

;; Handle a message from an actor
(define (behavior-handle-message beh msg)
  (let ((handler (lookup-handler beh (message-tag msg))))
    (log/receive (self) msg)
    (apply 
      (intercept handler 
                 (ask (get-mirror) receive msg handler))
      (message-arguments msg))))

;; Dynamic state of a running actor
(struct actor-state (self mirror fail))

;; A thread-local parameter to keep track of the actor state
(define *actor-state* (make-parameter (actor-state #f #f #f)))

;; install thread local state
(define (install-state! self mirror) 
  (*actor-state* (actor-state self mirror #f)))

;; Retrieve the `self` of the current actor 
(define (self)
   (ensure actor-state? (*actor-state*) "actor state should be initialized")
   (actor-state-self (*actor-state*)))

;; Checks whether the current actor has a mirror installed
(define (has-mirror) 
  (and (actor-state? (*actor-state*)) (actor-state-mirror (*actor-state*))))

(define get-mirror has-mirror)

;; Installs the failure continuation into the current actor's context 
(define (install-fail-continuation! cnt) 
  (ensure actor-state? (*actor-state*) "actor state should be initialized")
  (let ((original (*actor-state*)))
     (*actor-state* (actor-state (actor-state-self original)
                                   (actor-state-mirror original)
                                   cnt))))

;; Jumps back to the failure continuation with the 'fail value
;; effectively ends the current turn.
(define (fail-turn)
  ((actor-state-fail (*actor-state*)) 'fail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mirror API 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates an actor that is mirred by the given mirror
(define-syntax create-with-mirror
  (syntax-rules () 
     ((create-with-mirror mirror behavior arguments ...)
      (actor-create mirror behavior arguments ...))))

;; Changes the mirror of the current actor
(define (mirror! new-mirror)
  (ensure actor-state? (*actor-state*) "actor state should be initialized")
  (*actor-state* (actor-state (actor-state-self (*actor-state*))
                              new-mirror
                              (actor-state-fail (*actor-state*)))))

;; Equivalent to `actor`
(define-syntax mirror 
  (syntax-rules () 
    ((mirror name (constructor-parameter ...) (tag (parameter ...) body ...) ...)
     (behavior/macro name (constructor-parameter ...) (tag (parameter ...) body ...) ...))
    ((mirror (constructor-parameter ...) (tag (parameter ...) body ...) ...)
     (mirror 'none (constructor-parameter ...) (tag (parameter ...) body ...) ...))))

;; Causes the actor on the base level to error
;; Needs a reference to the ephemeral actor spawned by the base interpreter
(define (base/fail interpreter message) 
  (send interpreter fail message)
  (fail-turn))

;; Base level reply, circumvents any installed mirror
(define-syntax base/reply 
  (syntax-rules () 
    ((base/reply receiver arguments ...) 
     (intercept 
       (reply receiver arguments ...)
       (reply receiver arguments ...)))))

;; Base level send, circumvents any installed mirror
(define-syntax base/send 
  (syntax-rules ()
    ((send actor tag argument ...)
     (intercept
        (actor-send actor (quote tag) (list argument ...))
        (actor-send actor (quote tag) (list argument ...))))))

;; Creates a new mirror with an actor without going through the meta-layer
(define (base/create-with-mirror mirror behavior . arguments)
  (intercept 
     (apply actor-create mirror behavior arguments)
     (apply actor-create mirror behavior arguments)))

;; Creates a new actor without going through the meta-layer
(define (base/create behavior . arguments) 
  (apply actor-create #f behavior arguments))


;; Sends the message that is contained in the envelope to the receiver in the envelope
(define (base/send-envelope envelope)
  (ensure envelope? envelope "should be an envelope")
  (actor-send-message 
              (envelope-receiver envelope)
              (envelope-message envelope)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mirror Internals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keeps track of the meta-level for each thread
(define *meta-level* (make-parameter 0))

;; Computes the current meta-level of the interpreter
(define (current-meta-level) (*meta-level*))

;; Shorthand for current-meta-level
(define meta current-meta-level)

;; Update the current meta-level
(define (set-meta! new-meta)
  (*meta-level* new-meta))

;; Runs f in an increased meta-level, and restores the meta-level after executing f.
;; with-meta expects f to be a thunk (i.e., a function that does not accept any parameters)
(define (with-meta f)
  (set-meta! (+ (meta) 1))
  (let ((result (f)))
    (set-meta! (- (meta) 1))
    result))

(define-syntax intercept
  (syntax-rules () 
    ((intercept no-meta meta) 
     ;; avoid duplicating side-effects by binding and thunkify the expressions
     (let ((no-meta$ (lambda () no-meta))
           (meta$ (lambda () meta)))

       ;; if we are no longer on the base level execute the no-meta$ action, otherwise execute the meta action
       (if (> (current-meta-level) 0)
           (no-meta$)
           (if (has-mirror) (with-meta meta$) (no-meta$)))))))
