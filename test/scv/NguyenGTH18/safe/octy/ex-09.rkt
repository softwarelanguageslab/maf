;; TODO see ex-02, but probably that something is else going on here too,
;; must be checked

(define (f x)
    (if (number? x) (add1 x) (string-length x)))

(define (g x)
  (if (letrec (tmp (number? x))
	(if tmp tmp (string? x)))
      (f x)
      0))


(provide/contract
 (f (-> (or/c string? number?) number?))
 (g (-> any?  number?)))

(@unchecked f OPQ)
(@unchecked g OPQ)
(safe)
