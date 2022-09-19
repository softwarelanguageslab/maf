#lang racket


(require acontracts/actors)
(require acontracts/contracts)

;; This module provides multiparty session contracts.


;; A protocol is described by an actor behavior for representing the current state of the session. 
;; The in-memory representation of an a protocol is a constructor for such a behavior that will 
;; be called with all the roles of the participating actors.
(struct protocol/c (behavior-constructor))

;; A protocol describes a session between two or more actors.
;; We say that the protocol is the description of the session, whereas the session itself is a an instance of the protocol.
;; At any given point in time a session has only a single state. 
;; Transitions can be defined that change the state of the session, which in turn changes which transitions are allowed.
;; Transitions take the form of message sends between two actors. 
;; Each message interaction can be described by a message/c contract. Therefore all the patterns defined using mesasge/c contracts can be used to describe the interaction between two parties.
(define-syntax protocol/c
  (syntax-rules ()
    ((protocol/c (role ...) 
                 initial-behavior
                 (name behavior) ...)
     (protocol/c* 
       (let 
         ((name behavior) ...)
         (lambda (role ...) 
           initial-behavior))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Enforce 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enforce-sender contract actual-sender) 'todo)
(define (enforce-recipient contract actual-recipient) 'todo)
(define (enforce-contract-on-message contract actual-message) 'todo)

;; `enforce` always returns a behavior waiting for a matching message. 
(define (enforce contract next-behavior)
  (behavior '()
      (make-handlers 
        (list 
          ;; Catches the message that has been defined in the contract
          (cons (enforce-tag contract) 
                (lambda (sender recipient message)
                  ;; check whether the sender and recipient matches the contract
                  (enforce-sender contract sender)
                  (enforce-recipient contract self)
                  ;; check the contract itself on the messag"
                  (enforce-contract-on-message contract message)
                  ;; if all checks clear then the message can be forwarded to the actual recipient 
                  (send-envelope (envelope recipient message sender))
                  ;; then proceed to the next handler 
                  next-behavior))
          ;; catch all handler
          (cons '_ (lambda (sender recipient message)
                     (enforce-error "invalid message")))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quantifiers: forall and one-of
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; forall behaves a like sequence/c but uses a list of actors as a 
;; source, instead of a list of statements.
;;
;; @param actors is a non-empty list of actors
;; @param contract-constructor is a function of one arguments: the current actor and a continuation function
(define (forall actors contract-constructor)
  ;; nesting
  (define (nest actors)
    (if (null? actors)
        ;; if we are at the end of the actor list then all contracts 
        ;; have been validated. 
        '()
        ;; otherwise, we need to construct the contract for the given actor
       (contract-constructor (car actors) (lambda () 
                                            (nest (cdr actors))))))

  ;; type checks 
  (cond 
    [(not (list? actors)) (error "forall requires a list of actors")]
    [(null? actors) (error "the list of actors cannot be empty")]
    ;; in all other cases the type of the list should be fine
    [else (nest actors)))

;; One-of is exported from acontracts/contracts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `sequence/c` is an operator that transforms its sequence 
;; of statements in a series of nested `behavior` blocks.
;;
;; Any statements in a sequence/c block are automatically enforced.
;; 
;; The transformation is not much dissimilar of a CPS transformation.
(define-syntax (sequence/c stx)
  (let ((datum (syntax->datum stx)))
    (define (nest expressions)
      (if (null? expressions)
          ;; TODO: implement an empty-message contract
          '(enforce empty-message/c '())
          `(enforce ,(car expressions) 
                    ,(nest (cdr expressions)))))
    (datum->syntax stx (nest datum))))
