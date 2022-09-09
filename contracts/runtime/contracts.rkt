#lang racket

(require "./actors.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contract foundations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Message that carries an additional contract to be checked at the receiver side.
(struct contract-message message (receiver-contracts) )

;; Checks whether the given message is allowed to be sent by the given contract
(define (is-allowed-to-sent? receiver-contract message)
  'todo
  #t)

;; Checks the contract against the messages that have been send during the execution of the message handler
(define (sent-history-satisfies-contract? receiver-contract sent-messages)
  'todo
  #t)

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
    (receive (interpreter msg handler) (base/fail "could not handle message while processing a message"))
    ;; Track which messages the base actor sends
    (send (interpreter envelope)
      (if (is-allowed-to-send? contract (envelope-message envelope))
          (begin 
             (base/send-envelope envelope)
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
    (send (interpreter envelope)
      ;; Check whether the send side contract of the actor have been satisfied 
      'todo
      (base/reply interpreter 'ok)
      (base/send-envelope envelope)
      (become base-contracts))
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
          
(define (enable-contracts!)
  (install-mirror! (create base-contracts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sender side contracts 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; behavior/C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Receiver side contracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ensures/c
