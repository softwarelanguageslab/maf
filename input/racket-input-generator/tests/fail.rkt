#lang racket

;; test whether if a contract fails we get sufficient information to generate new values

(define/contract (foo x)
   (-> number? number?)
   x)

(struct posn (x y) #:transparent)
(define POSN/C (struct/c posn number? number?))

(for/list ([i (in-range 10)]) 
   (contract-random-generate (-> (listof POSN/C) number?)))
