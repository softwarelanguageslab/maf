; OK

(define (f x y)
  (if (and (number? x) (string? y)) (+ x (string-length y)) 0))

(provide/contract 
  (f (-> any? any? number?)))

(@unchecked f OPQ OPQ)
(safe)
