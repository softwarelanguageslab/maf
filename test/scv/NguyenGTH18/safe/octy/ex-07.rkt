;; OK

(define (f x y)
  (if (if (number? x) (string? y) #f)
      (+ x (string-length y))
      0))


(provide/contract 
  (f (-> any? any? number?)))

(f OPQ OPQ)
(safe)
