(define (bar v n m)
  (cond [(and (zero? v) (> n 0)) (bar m m m)]
        [(> v 0) 0]
        [else 0]))

(provide
 (contract-out
  [bar (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? . -> . exact-nonnegative-integer? #:total? #t)]))
