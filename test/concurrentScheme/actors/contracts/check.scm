#lang racket 
(parse-cmdline!)
(require acontracts)

;; Benchmark showing that when actors are allocated at the same call site, precision is lost.

(define check-actor 
    (actor "check" (n)
           (check (m)
                  (if (= m n) (become check-actor n) (error "error!")))))

(define check/c (behavior/c (integer?) 
                  (check (integer?) unconstrained/c)))

(define new-check (lambda (n) (create/c check/c check-actor n)))

(define c1 (new-check 1))
(define c2 (new-check 2))
(define c3 (new-check 3))

(send c1 check 1)
(send c2 check 2)
(send c3 check 3)

(print-statistics)
