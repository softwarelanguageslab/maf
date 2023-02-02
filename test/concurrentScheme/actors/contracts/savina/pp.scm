#lang racket
 
(require acontracts)
(parse-cmdline!)


;; Adapted from Savina benchmark ("Ping Pong" benchmarks, coming from Scala)
(define ping-actor
  (actor "ping" (count pong)
           (start ()
                  (send pong send-ping self)
                  (become ping-actor (- count 1) pong))
           (ping ()
                 (send pong send-ping self)
                 (become ping-actor (- count 1) pong))
           (send-pong ()
                      (if (> count 0)
                          (begin
                            (send self ping)
                            (become ping-actor count pong))
                          (begin
                            (send pong stop)
                            (terminate))))))
(define pong-actor
  (actor "pong" (count)
           (stop () (terminate))
           (send-ping (to)
                      (send to send-pong)
                      (become  pong-actor (+ count 1)))))


;; TODO: actually here we can demonstrate the recursive nature of the contracts
;; i.e., instead of having unconstrained/c at the end another behavior contract can be used 
;; to ensure that the receiving actor either sends ping (to itself) or stop to us
(define pong/c (behavior/c (integer?)
   (stop () unconstrained/c)
   (send-ping (actor?) (lambda _ (ensures/c (send-pong () unconstrained/c))))))

(define ping/c (behavior/c (integer? pong/c)
   (start       () (lambda _ (ensures/c (send-ping (actor?) unconstrained/c))))
   (ping        () (lambda _ (ensures/c (send-ping (actor?) unconstrained/c))))
   ;; TODO: allow either ping or stop
   (send-pong   () unconstrained/c)))

(define pong (create/c pong/c pong-actor 0))
(define N 500)
(define ping (create/c ping/c ping-actor N pong))
(send ping start)

(print-statistics)
