#lang racket

(provide 
  ask/c* 
  ask/c)

(require acontracts)

;; The interaction patterns require the contract system to be enabled
(enable-contracts!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ask pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: it is probably also interesting to encode that the sending actor of the message
;; is different from the empheral actor? Ideally we can encode that it has created an actor 
;; to receive the reply, but that might be difficult? 

;; An actor satisfies this contract is it understands the "answer" message.
(define (answer-actor/c return-value) 
  (behavior/c () 
   (answer (return-value) unconstrainted/c)
   (_ (lambda idc unconstrained/c))))

;; A contract that encodes the "ask" pattern
;; Usage (desugared): 
;; (ask/c* (quote tag) (list argument-contracts ...) return-contract)
(define (ask/c* tag arguments return-contract)
  (message/c* tag (cons (answer-actor/c return-contract) arguments) 
             (lambda (arguments)
                (let ((recipient (car arguments)))
                  (ensures/c 
                    ;; ensure that the empheral actor receive the correct value
                    (answer (return-contract) (specific-recipient recipient)))))))

;; Syntax version of ask/c
(define-syntax ask/c 
  (syntax-rules ()
    ((ask/c tag (argument-contract ...) return-contract)
     (ask/c* (quote tag) (list argument-contract ...) return-contract))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aggregator pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pull Pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
