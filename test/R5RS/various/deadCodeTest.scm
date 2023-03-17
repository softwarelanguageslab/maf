(define (called n)
  (+ 1 1)
  n)

(define (uncalled n)
  n)

(if #t
    (called 10)
    (uncalled 11))