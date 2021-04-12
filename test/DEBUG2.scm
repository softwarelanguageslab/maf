(define (odd? m)
  (if (<= m 0)
      #f
      (even? (- m 1))))

(define (even? n)
  (if (<= n 0)
      #t
      (odd? (- n 1))))

(odd? (<change> 10 10.5))