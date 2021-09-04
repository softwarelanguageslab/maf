;; NOT OK: f does not satisfy its output contract, although it can never
;; be called with a valid value. 
;;
;; not sure how this can be fixed

(define (f x) 5)

(provide/contract
  (f (-> (and/c number? string?) string?)))

(@unchecked f OPQ)
(safe)
