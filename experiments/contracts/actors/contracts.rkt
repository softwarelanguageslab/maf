#lang racket

(require "./base.rkt")

(provide 
  enable-contracts!
  create/c
  ->m
  behavior/c)

(struct behavior-contract (cstors msgs name))

(define/contract (behavior/c-mirror contract)
  (-> behavior-contract? actor?)
  (create (behavior contract ((_ any?)) 
    (messages/extend (base-mirror base-mirror) ())) '()))

;; a contract on the handling of a message.
;; properties of interest are: existance of message handler .
;; ensuring that particular message sends happen.
;; ensuring that the actor ends up in a particular behavior.
(struct message/c (parameter-contracts expected-tag ensure-sends ensure-behaviors))

;; Returns the contracts on the arguments of a message 
(define (message-contract-ref message-contracts tag)
  (message/c-parameter-contracts (hash-ref message-contracts tag)))
  
;; a contract on a message send 
(struct m-> (tag contracts))

(define-syntax ->m
  (syntax-rules ()
    ((->m tag contracts ...) (m-> (quote tag) (list contracts ...)))))

(struct contracted-actor (behavior-contract actor))

;; enforces the contracts in a message send. Is executed on the node that is sending the message.
(define (enforce-message-send receiver tag arguments)
  (let*
    ((behavior-contract (contracted-actor-behavior-contract receiver))
     (message-contracts (behavior-contract-msgs behavior-contract))
     (message-argument-contracts (message-contract-ref message-contracts tag)))

    (for-each (lambda (argument c)
                (contract c argument 'pos 'neg))
              arguments 
              message-argument-contracts)))

;; This mirror intercepts `send` instructions on of the actor it is installed in. 
;; The purpose of intercepting this messages is to extend `send` with support for contracted actors, 
;; such that a particular set of contracts is checked at the sender site.
(define (contract-system-mirror previous-mirror)
  (letrec ((contract-mirror (behavior "contract-system-mirror" ((_ any?))
      (messages/extend (base-mirror contract-mirror)
           ((send (sender receiver tag arguments)
            ;; if the receiver is a contracted actor reference then we need to apply the contracts
            ;; and wrap any ensure/c contracts in a contracted message. 
            (debug "meta/send")
            (let 
              ((actor (if (contracted-actor? receiver)
                (begin 
                   (enforce-message-send receiver tag arguments)
                   (contracted-actor-actor receiver))
                receiver)))
            (reply sender (send-meta actor (message tag arguments)))
            (become contract-mirror contract-mirror))))))))
    (create-with-mirror previous-mirror contract-mirror contract-mirror)))
                
(define (enable-contracts!)
  (default-mirror! contract-system-mirror)
  (install-mirror))

(define-syntax behavior/c 
  (syntax-rules (ensure-send)
     ((behavior/c name (constructor-contract ...) 
                 ((tag (param-contract ...) (ensure-send send ...) (ensure-becomes become ...)) ...))
      (behavior-contract (list constructor-contract ...) 
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

(define-syntax create/c
  (syntax-rules () 
    ((create/c contract actor arguments ...) 
     (let ((contract$ contract))
     (contracted-actor contract$
       (create-with-mirror (behavior/c-mirror contract$) actor arguments ...))))))
