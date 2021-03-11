;; TODO 
;; the issue here is that it has no knowledge about the or when analysing the function
;; so it cannot verify this correctly
;
(define (f x)
  (if (number? x) (add1 x) (string-length x)))

(provide/contract 
  (f (-> (or/c string? number?) number?)))

(@unchecked f OPQ)
(safe)
