;; OK

(define (tak x y z)
  (if (< y x) ;; original: (false? (< y x)), swapped consequent and alternative
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))
      z))

(provide/contract
 (tak (-> int? int? int? int?)))

(@unchecked tak OPQ OPQ OPQ)
(safe)
