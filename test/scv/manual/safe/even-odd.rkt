
(define (even x)
  (if (= x 0)
      #t
      (odd (- x 1))))

(define (odd x)
  (if (= x 0)
      #f
      (even (- x 1))))

(provide (contract-out 
           [even (-> integer? integer?)]))
