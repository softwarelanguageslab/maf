#lang racket
(require soft-contract/fake-contract)

(define (strnum? x)
  #|HERE|# (number? x))

(provide/contract
 [strnum? (->i ([x any/c]) (res (x) (and/c boolean? (λ (a) (equal? a (or (string? x) (number? x)))))))])
