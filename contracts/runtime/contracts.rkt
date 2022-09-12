#lang racket

(require "./actors.rkt")
(provide 
  enable-contracts!
  behavior/c
  behavior/c* 
  message/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contract foundations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Message that carries an additional contract to be checked at the receiver side.
(struct contract-message message (receiver-contracts) )

;; An actor reference that has a contract attached to it
;; Usually created by create/c
(struct contracted-actor actor (contract))

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
      (base/reply interpreter (base/create-with-mirror (base/create base-contracts) beh arguments))
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
          (begin 
             (base/fail interpreter "not allowed to send")
             (become message-send-tracker contract sent-messages))))
    ;; Must be sent in order finalize the tracker and check whether all the contracts have been satisfied 
    ;; by the message sent history.
    (check-contract () 
       (unless (sent-history-satisfies-contract? contract sent-messages)
         (base/fail interpreter "message history did not satisfy the contract")
         (become base-contracts)))))
           
(define base-contracts 
  (mirror "base-contracts" () 
    ;; Intercepting "create" allows us to install the contract mirror on newly created actors
    (create (interpreter beh arguments) 
      ;; We are using base/* versions of all the actor related primtiives in order to directly go to the base layer 
      (base/reply interpreter (base/create-with-mirror (base/create base-contracts) beh arguments))
      (become base-contracts)
    ;; Intercepting "send" allows us to send messages to other types of actors, and check whether 
    ;; the arguments of the message satisfy the message contract before sending them.
    (send (interpreter envelope) (base-intercept-send interpreter envelope))
    (receive (interpreter msg handler)
      ;; If the message is a contracted message we should return a different handler.
      ;; The handler should check whether the contract have been satisfied.
      (if (contracted-message? msg) 
          (begin 
            (reply-to interpreter (lambda args 'todo))
            (base/become message-send-tracker (contracted-message-contract msg)))
          (begin 
            (reply-to interpreter handler)
            (become base-contracts)))))))

;; Base behavior for the contract system when a message is being send.
(define (base-intercept-send interpreter envelope)
   (let* ((receiver  (envelope-receiver envelope))
          (message   (envelope-message envelope))
          (tag       (message-tag message))
          (arguments (message-arguments message)))

      ;; if the receiver is a contracted actor, we need to check whether the send side contracts have been satisfied
      (let*
        ((message (if (contracted-actor? receiver)
          (contracted-message message (check-sender-contracts interpreter (contracted-actor-contract reveiver) tag arguments))
          message))
         (envelope (envelope receiver message)))


         ;; continue with the regular send
         (base/reply interpreter 'ok)
         (base/send-envelope envelope)
         (become base-contracts))))

          
(define (enable-contracts!)
  (install-mirror! (create base-contracts)))

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

;; Syntactic sugar support
(define-syntax behavior/c 
  (syntax-rules ()
    ((behavior/c (tag (argument-contract ...) behavior-contract) ...)
     (behavior/c* (list (message/c (quote tag) (list argument-contract ...) behavior-contract) ...)))))

;; A contract on a specific message.
(struct message/c (tag arguments behavior-contract))

;; Checks whether the given message contract matches the given tag
(define (matches-tag mesasge-contract tag)
  (eq? (message/c-tag message-contracts) tag))

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
  (contract cont argument 'pos 'neg))

;; Checks whether the sender contracts are correct
(define (check-sender-contracts interpreter contract tag arguments)
  (let ((contract (find-matching-contract (behavior/c*-messages contract) tag)))
    (if (not contract)
        ;; TODO: make the error message more precise such that it can be used for soundness testing
        ;; and it provides accurate blame information.
        (base/fail interpreter "actor does not respond to message")
        ;; the contract can be found so we can check whether the arguments satisfy the contract
        (let ((argument-contracts (message/c-arguments contract)))
          ;; if one of the contracts is not satisfied we should signal an error
          (if (forall (map apply-contract argument-contracts arguments))
            (begin 
               ;; TODO: provide more accurate blame information about the contract violation
              (base/fail interpreter "contract violated")
              #f)
            (message/c-behavior-contract contract))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Receiver side contracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ensures/c

;; Ensures that all the given messages are send while processing a particular message
;; Usage (without syntax sugering): 
;; (ensures/c* (list (message/c 'display (list integer?))))
;;
;; With sugaring:
;; (ensures/c ((display (integer?))))
(struct ensures/c* (messages))

;; Syntactic sugar support
(define-syntax ensures/c 
  (syntax-rules ()
    ((ensures/c ((tag (argument-contract ...)) ...))
     (ensures/c* (list (message/c (quote tag) (list argument-contract ...)) ...)))))

;; Checks whether the given message is allowed to be sent by the given contract
(define (is-allowed-to-sent? receiver-contract message)
  ;; currently ensures/c does not impose any restrictions on which messages are allowed to be sent
  #t)

;; Checks the contract against the messages that have been send during the execution of the message handler
(define (sent-history-satisfies-contract? receiver-contract sent-messages)
  ;; we currently only support checking contracts of the ensures/c* type 
  (unless (ensures/c receiver-contract)
    (error "not an ensures/c contract"))
  ;; the ensures/c contract checks whether the actor has sent all the required messages 
  (let 
    ;; todo: also check message arguments, but that should perhaps ve done in is-allowed-to-sent? 
    ((all-tags (map message/c-tag (ensures/c-messages receiver-contract))))
    (forall (map (lambda (v) (member all-tags v)) (map message-tag sent-messages)))))
