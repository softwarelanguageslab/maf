;; TODO push down the verification of the range contract to the component 
;; under analysis such that we can re-use the path conditions contained within

(define (recip x)
  (if (and (number? x) (not (zero? x)))
      (/ 1 x)
      "expect non-zero number"))

(provide/contract 
  (recip (-> any/c (or/c (and/c number? (not/c zero?)) string?))))

(@unchecked recip OPQ)
(safe)
