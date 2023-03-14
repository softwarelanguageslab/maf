#lang racket 

(require acontracts)
(parse-cmdline!)

(define Limit 10000)
(define NumMaxLocalPrimes 5)

(define (locally-prime n local-primes) 
  (if (null? local-primes)
      #t 
      (if (= 0 (remainder n (car local-primes)))
          #f 
          (locally-prime n (cdr local-primes)))))

(define number-producer-actor
  (actor "number-producer-actor" ()
           (prime-filter (actorRef)
                         (letrec ((loop (lambda (candidate)
                                          (if (>= candidate Limit)
                                              (begin
                                                (send actorRef exit)
                                                (terminate))
                                              (begin
                                                (send actorRef candidate candidate)
                                                (loop (+ candidate 2)))))))
                           (loop 3)))))
(define prime-filter-actor
  (actor "prime-filter-actor" (id initial next local-primes available-local-primes)
           (candidate (candidate)
                      (if (locally-prime candidate local-primes)
                          (if next
                              (begin
                                (send next candidate candidate)
                                (become prime-filter-actor id initial next local-primes available-local-primes))
                              (if (< available-local-primes NumMaxLocalPrimes)
                                  (become prime-filter-actor id initial next (cons candidate local-primes) (+ available-local-primes 1))
                                  (let ((new-next (create prime-filter-actor (+ id 1) candidate #f (cons candidate '()) 1)))
                                    (become prime-filter-actor id initial new-next local-primes available-local-primes))))
                          (become prime-filter-actor id initial next local-primes available-local-primes)))
           (exit ()
                 (if next
                     (send next exit)
                     '())
                 (terminate))))


(define prime-filter-actor/c 
  (behavior/c (any/c any/c any/c any/c any/c)
    (candidate (integer?)  unconstrained/c)
    (exit      () unconstrained/c)))

;; a producer is supposed to produce a number of candidates and end with exit
(define producer/c 
  (behavior/c ()
    (prime-filter (prime-filter-actor/c) 
                  (lambda _
                     (sequence/c (repeated/c (message/c candidate (integer?) unconstrained/c))
                                 (single/c   (message/c exit () unconstrained/c)))))))

(define producer (create/c producer/c number-producer-actor))
(define filter (create/c prime-filter-actor/c prime-filter-actor 1 2 #f (cons 2 '()) 1))
(send producer prime-filter filter)

(print-statistics)
