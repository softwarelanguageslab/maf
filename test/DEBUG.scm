(define A
  (<change>
    (lambda () #t)
    (lambda () (= 0 0))))
(define (B)
  (not (A)))
(B)
