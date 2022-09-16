#lang racket

(provide 
  ask/c* 
  ask/c)

(require acontracts)

;; The interaction patterns require the contract system to be enabled
(enable-contracts!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request-Reply Pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A request and reply-pattern, similar to the ask/c pattern 
;; but requires that the response is sent to the same actor as the sender
(define (request-reply* request arguments reply return-contract from to)
  (define requester/c (behavior/c* 
                        (message/c* reply (list return-contract)
                                   unconstrained/c 
                                   any-recipient)))

   ;; message should be tagged with the given request tag
  (message/c* request
              ;; the response should go to the sender and the 
              ;; sender should be able to handle the answer message
              (cons (and/c self?/c requester/c) arguments)
              ;; make sure that the server sends a reply back
              (lambda (arguments)
                (let ((recipient (car arguments)))
                  (ensures/c 
                    (message/c reply (list return-contract) 
                               unconstrained/c
                               (specific-recipient recipient)))))
              from 
              to))

;; Syntactic sugar for the request-reply* function
(define-syntax request-reply
  (syntax-rules () 
    (request-reply request-tag (arguments ...) reply-tag return-contract)
    (request-reply* (quote request-tag) (list arguments ...) (quote reply-tag) return-contract)))

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

;; The aggregator pattern has the following roles:
;; - a requester that request that resources from the aggregator 
;; - the aggregator itself 
;; - a number of services that are queried using the aggregator 

(define (aggregator/c request request-arguments)
  (protocol/c (requester aggregator services)
      start
      [start 
        ;; the aggreagot should when receiving a request 
        ;; query the aggregated services 
        (message/c* request request-arguments 
                   (ensures/c* (forall services (lambda (service)
                                                  (message/c* message/any 
                                                              arguments/any 
                                                              (lambda idc unconstrained/c )
                                                              aggregator
                                                              (specific-recipient service))))))
        (become wait-for-services)]
      [wait-for-services
        ;; then it should wait for the services to respond
        (sequence 
          (forall* services (lambda (service)
                           (message/c* message/any 
                                       arguments/any
                                       (lambda idc unconstrained/c)
                                       service
                                       aggregator)))
           ;; then send the aggregated data back to the requester
           (message/c* (quote reply) arguments/any (lambda idc unconstrained/c) aggregator requester)
           ;; terminate the session
           (terminate))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pull Pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In the pull pattern we have one (arbitrarily fast) master and multiple workers.
;; The idea of the contract is to identify the master and the workers and specify the exchange of messages

(define (pull/c* get-work work-tag work-type)
  (protocol/c (master workers)
     ;; default behavior of the protocol
     loop
     ;; the worker sends a message to the master to get some work, and gets work back, all during the same turn of the master
     [loop 
       (request-reply* get-work '() work-tag work-type (one-of workers) master)
       (become loop)]
     ;; messaging the master to terminate 
     ;; it should then message all workers that they also terminate 
     [terminate 
       (forall workers (lambda (worker) (message/c terminate () unspecified/c master worker)))]))
