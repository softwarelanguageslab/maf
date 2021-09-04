;; TODO this cannot be verified, but can be if =/c would be part 
;; of the component of f
(define (f x) x)

(provide/contract
  (f (~ number? (lambda (x) (=/c x)))))

;(provide/contract [f (->i ([x real?]) (res (x) (=/c x)))])
(@unchecked f OPQ)
(safe)

