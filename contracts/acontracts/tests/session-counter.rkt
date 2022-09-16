#lang racket

;; This module defines a contract that specifies an interaction between three actors:
;; - A counter that keeps track of a single counter value (defined as `counter` below)
;; - A produces which sends increment reqursts to the counter actor 
;; - A consumer which periodically asks for the value of the counter, and also gets the value of the counter after it has been incremented

(require acontracts)

(define counter (actor (consumer vlu) 
                  (increment (v)  
                     (send consumer vlu)
                     (become counter (+ vlu v)))))

(define producer (actor (counter-actor times-executed) 
                   (do-it () 
                          (if (= times-executed 10)
                              (begin 
                                (send counter-actor terminate)
                                (send interval-handle terminate)
                                (terminate))
                              (send counter-actor increment 1)))))

(define consumer (actor ()
                   (value (v) (displayln v))))

;; `every-seconds` is a function in the actor library that allows a specific to be executed every X seconds
(define interval-handle (every-seconds 1 (lambda () (send producer do-it))))
                  
;; A counter protocol, it has three roles:
;; - a counter which accepts increment messages 
;; - a producer which sends increment messages 
;; - a consumer which receives a message increment after the message has been incremented
;; repeat ad infinitum until a message "end" is being sent by the producer
(define counter/c 
  (protocol/c (counter producer consumer)
    (init
       [(message/c (increment (integer?)) producer consumer)
        (message/c (value (integer?) unconstrained/c) counter consumer)
        (become init)]
       ;; if we send "do-it" to the producer, and if it chooses to terminate then it should also terminate the other actors
       [(message/c (do-it ()) unspecified-sender producer)
        ;; ensure that the producer sends termination messages
        (message/c (terminate ()) producer counter)
        (message/c (terminate ()) producer consumer)
        ;; ensure that all actors have terminated
        (terminated producer)
        (terminated consumer)
        (terminated counter)
        ;; terminate the protocol
        (terminate)])))

