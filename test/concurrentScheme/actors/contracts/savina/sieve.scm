(define Limit (int-top))
(define NumMaxLocalPrimes (int-top))

(define (locally-prime n) (bool-top)) ;; not modeled
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
                      (if (locally-prime candidate)
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
                     (send next exit))
                 (terminate))))

(define producer (create number-producer-actor))
(define filter (create prime-filter-actor 1 2 #f (cons 2 '()) 1))
(send producer prime-filter filter)
