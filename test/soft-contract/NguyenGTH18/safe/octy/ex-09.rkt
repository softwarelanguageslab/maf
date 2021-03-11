;; TODO see ex-02, but probably that something is else going on here too,
;; must be checked

(define (f x)
    (if (int? x) (add1 x) (string-length x)))

(define (g x)
  (if (letrec (tmp (int? x))
	(if tmp tmp (string? x)))
      (f x)
      0))


(provide/contract
 (f (-> (or/c string? int?) int?))
 (g (-> any?  int?)))

(@unchecked f OPQ OPQ)
(@unchecked g OPQ)
(safe)
