;; TODO two improvements can be made here:
;; (1) making sure that or/c generates an or in the SMTlib, should that 
;;     can reason about the fact that if (number? extra1) but not (number? input)
;;     then (string? input) must be valid.
;; (2) path reasoning based on the `and` special form, such that it can collect
;; information about the fact that (number? (car extra)) is true

(define (f input extra)
  (letrec (extra1 (car extra))
   (cond
      ((and (number? input) (number? extra1))
      (+ input extra1))
      ((and (string? input) (number? extra1))
      ;; old: ((number? (car extra)) see TODO (1)
      (+ (string-length input) extra1))
      (else 0))))

(provide/contract
    (f (-> (or/c number? string?) pair? number?)))

(@unchecked f OPQ OPQ)
(safe)
