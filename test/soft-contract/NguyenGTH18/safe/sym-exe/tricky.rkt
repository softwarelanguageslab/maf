(define (f x)
  (if (zero? x) 0
      (if (zero? (f (sub1 x))) 7 8)))

(provide/contract 
  (f (-> int?  int?)))

(@unchecked f OPQ)
(safe)
