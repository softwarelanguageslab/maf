;; OK; changed number? to number?
;; TODO support number? as well

(define (f x)
  (if (number? x) (add1 x) 0))

(provide/contract 
  (f (-> any? number?)))

(@unchecked f OPQ)
(safe)
