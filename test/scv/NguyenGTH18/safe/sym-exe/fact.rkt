(define (factorial x) 
  (if (zero? x) 
      1
      (* x (factorial (sub1 x)))))

(provide/contract
  (factorial (-> (>=/c 0) (>=/c 0))))

(@unchecked factorial OPQ)
(safe)
