#lang racket/base

(require soft-contract/fake-contract)

(define (gcd a b)
  (cond [(zero? b) a]
        [else (gcd b (modulo a b))]))

(provide
 (contract-out
  [gcd (exact-nonnegative-integer? exact-nonnegative-integer? . -> . exact-nonnegative-integer? #:total? #t)]))
