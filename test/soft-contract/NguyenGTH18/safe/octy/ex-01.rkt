;; OK; changed int? to int?
;; TODO support int? as well

(define (f x)
  (if (int? x) (add1 x) 0))

(provide/contract 
  (f (-> any? int?)))

(@unchecked f OPQ)
(safe)
