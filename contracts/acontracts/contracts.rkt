#lang racket

(require (for-syntax syntax/parse))
(require acontracts/actors) 
(provide 
  enable-contracts!
  behavior/c
  behavior/c* 
  ensures/c 
  ensures/c*
  message/c
  create-with-contract
  unconstrainted/c
  any-recipient 
  specific-recipient
  (rename-out (create-with-contract create/c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contract foundations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Message that carries an additional contract to be checked at the receiver side.
(struct contracted-message message (contract) #:transparent)

;; An actor reference that has a contract attached to it
;; Usually created by create/c
(struct contracted-actor actor-struct (contract))

;; Create an actor that has the given contract attached to it
(define-syntax create/c
  (syntax-rules ()
     ((create/c contract actor argument ...)
      (contract-actor (create actor argument ...) contract))))

;; Mirror behavior that tracks which messages an actor has sent.
(define message-send-tracker 
  (mirror "message-send-tracker" (contract sent-messages) 
    ;; Same behavior as "base-contracts"
    (create (interpreter beh arguments) 
      (base/reply interpreter (apply base/create-with-mirror (base/create base-contracts) beh arguments))
      (become message-send-tracker contract sent-messages))
    ;; Impossible to receive a message at this point in time since the base actor is processing one
    (receive (interpreter msg handler) (base/fail interpreter "could not handle message while processing a message"))
    ;; Track which messages the base actor sends
    (send (interpreter envelope)
      (if (is-allowed-to-send? contract (envelope-message envelope))
        (begin 
          (base-intercept-send interpreter envelope)
          (become message-send-tracker contract (cons (envelope-message envelope) sent-messages)))
        ;; Cause an error on the base-level if the send fails
        (base/fail interpreter "not allowed to send")))
    ;; Must be sent in order finalize the tracker and check whether all the contracts have been satisfied 
    ;; by the message sent history.
    (check-contract (interpreter) 
       (unless (sent-history-satisfies-contract? contract sent-messages)
         (base/fail interpreter "message history did not satisfy the contract"))
       (become base-contracts))))
           
(define base-contracts 
  (mirror "base-contracts" () 
    ;; Intercepting "create" allows us to install the contract mirror on newly created actors
    (create (interpreter beh arguments) 
      ;; We are using base/* versions of all the actor related primtiives in order to directly go to the base layer 
      (base/reply interpreter (apply base/create-with-mirror (base/create base-contracts) beh arguments))
      (become base-contracts))
    ;; Intercepting "send" allows us to send messages to other types of actors, and check whether 
    ;; the arguments of the message satisfy the message contract before sending them.
    (send (interpreter envelope) (base-intercept-send interpreter envelope))
    (receive (interpreter msg handler)
      ;; TODO: also pass in original behavior/c contract such that we can check whether we promised 
      ;; to be able to process a particular message.
      ;; If the message is a contracted message we should return a different handler.
      ;; The handler should check whether the contract have been satisfied.
      (if (contracted-message? msg) 
          (let ((mirror-self self)) 
            (reply-to interpreter (lambda args 
                                    ;; lambda executed locally on the base-level
                                    (let ((new-behavior (apply handler args)))
                                       ;; we create a temporary actor here such that the error is executed 
                                       ;; at the base level
                                       (let ((temporary (create (actor "temp" () (fail (m) (error m))))))
                                         (base/send mirror-self check-contract temporary)
                                         new-behavior))))
            (become message-send-tracker (contracted-message-contract msg) '()))
          (begin 
            (reply-to interpreter handler)
            (become base-contracts))))))

;; Base behavior for the contract system when a message is being send.
(define (base-intercept-send interpreter ev)
   (let* ((receiver  (envelope-receiver ev))
          (message   (envelope-message ev))
          (tag       (message-tag message))
          (arguments (message-arguments message)))

      ;; if the receiver is a contracted actor, we need to check whether the send side contracts have been satisfied
      (let*
        ((message (if (contracted-actor? receiver)
          (contracted-message tag arguments (check-sender-contracts interpreter (contracted-actor-contract receiver) tag arguments))
          message))
         (ev (envelope receiver message)))

         ;; continue with the regular send
         (base/reply interpreter 'ok)
         (base/send-envelope ev)
         (become base-contracts))))

          
(define (enable-contracts!)
  (mirror! (create base-contracts)))

(define-syntax create-with-contract
  (syntax-rules () 
    ((create-with-contract contract behavior arguments ...) 
     (contracted-actor 
       (actor-tid (create behavior arguments ...))
       contract))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sender side contracts 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; behavior/C

;; Usage (without syntactic sugar): 
;; (define counter/c (behavior/c*
;;                               (list (message/c 'increment (list actor? integer?) 
;;                                                           (ensures/c* (list (message/c 'display (list integer?))))))))
;; 
;; With syntactic sugar:
;; (define counter/c (behavior/c 
;;                      (increment (actor? integer?) (ensures/c ((display (integer?)))))))
(struct behavior/c* (messages))

(define-syntax (translate-message-contract stx)
  (syntax-parse stx
   ;; A catch-all pattern
   [((~literal _) behavior-contract) #'(message/c '() '() behavior-contract any-recipient)]
   ;; format as ocumented in behavior/c
   [(tag (argument-contract ...) behavior-contract) #'(message/c (quote tag) (list argument-contract ...) behavior-contract any-recipient)]
   ;; catch-all
   [contract contract]))

;; Syntactic sugar support
(define-syntax behavior/c 
  (syntax-rules ()
    ;; TODO: incorperate constructor-contract
    ;; TODO: match against "_" and translate to message-_/c
    ((behavior_c (constructor-contract ...) message-contract ...)
     (behavior/c* (list (translate-message-contract message-contract) ...)))))

;; A contract on a specific message.
(struct message/c (tag arguments behavior-contract intented-recipient) #:transparent)

;; A catch-all message
(define message-_/c (message/c '() '() (lambda idc unconstrained/c) #f))

;; Checks whether the given message contract matches the given tag
(define (matches-tag message-contract tag)
  (or (eq? (message/c-tag message-contract) '()) ;; matches any tag
      (eq? (message/c-tag message-contract) tag)))

;; Find the correct message contract in a list of message contracts for the given tag
;; Returns #f if no matching contract is found.
(define (find-matching-contract contracts tag)
  (if (null? contracts)
      #f 
      (if (matches-tag (car contracts) tag)
          (car contracts)
          (find-matching-contract (cdr contracts) tag))))

(define (apply-contract cont argument)
  ;; TODO: provide accurate blame information
  (cont argument))

;; Checks whether the sender contracts are correct
(define (check-sender-contracts interpreter contract tag arguments)
  (let ((contract (find-matching-contract (behavior/c*-messages contract) tag)))
    (if (not contract)
        ;; TODO: make the error message more precise such that it can be used for soundness testing
        ;; and it provides accurate blame information.
        (begin 
          (base/fail interpreter "actor does not respond to message"))
        ;; the contract can be found so we can check whether the arguments satisfy the contract
        (let ((argument-contracts (message/c-arguments contract)))
          ;; if one of the contracts is not satisfied we should signal an error
          (if (not (forall (map apply-contract argument-contracts arguments)))
            (begin 
               ;; TODO: provide more accurate blame information about the contract violation
              (base/fail interpreter "contract violated")
              #f)
            (begin
               ((message/c-behavior-contract contract) arguments)))))))

;; Represents any valid recipient
(define any-recipient (lambda (actual-actor) #t))

;; Represents a specific recipient
(define (specific-recipient actor) 
  (lambda (actual-actor) (eq? actor actual-actor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Receiver side contracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unconstrained/c 

;; This contracts does not put any additional constraints on the behavior of the actor 
(define unconstrained/c (ensures/c* '()))

;; ensures/c

;; Ensures that all the given messages are send while processing a particular message
;; Usage (without syntax sugering): 
;; (ensures/c* (list (message/c 'display (list integer?))))
;;
;; With sugaring:
;; (ensures/c ((display (integer?))))
(struct ensures/c* (messages) #:transparent)

;; Syntactic sugar support
(define-syntax ensures/c 
  (syntax-rules ()
    ((ensures/c (tag (argument-contract ...) intended-recipient) ...)
     (ensures/c* (list (message/c (quote tag) (list argument-contract ...) (lambda idc unconstrained/c) intended-recipient) ...)))))


;; Checks whether the given message is allowed to be sent by the given contract
(define (is-allowed-to-send? receiver-contract message)
  ;; currently ensures/c does not impose any restrictions on which messages are allowed to be sent
  #t)

;; Checks the contract against the messages that have been send during the execution of the message handler
(define (sent-history-satisfies-contract? receiver-contract sent-messages)
  ;; we currently only support checking contracts of the ensures/c* type 
  (unless (ensures/c*? receiver-contract)
    (error "not an ensures/c contract"))
  ;; the ensures/c contract checks whether the actor has sent all the required messages 
  (let 
    ;; todo: also check message arguments, but that should perhaps ve done in is-allowed-to-sent? 
    ((all-tags (map message/c-tag (ensures/c*-messages receiver-contract))))
    (or (null? all-tags) 
        (and (not (null? sent-messages)) 
         (forall (map (lambda (v) (member all-tags v)) (map message-tag sent-messages)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful (small) contracts 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (self?/c actor)
  (eq? self actor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (forall booleans) 
  (if (null? booleans)
      #t
      (and (car booleans) 
           (forall (cdr booleans)))))
