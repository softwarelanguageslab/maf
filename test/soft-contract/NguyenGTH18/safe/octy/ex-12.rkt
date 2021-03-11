#lang racket

(define (carnum? p) (number? (car p)))

(provide/contract
 [carnum? (->i ([p pair?]) (res (p) (and/c boolean? (λ (a) (equal? a (number? (car p)))))))])
