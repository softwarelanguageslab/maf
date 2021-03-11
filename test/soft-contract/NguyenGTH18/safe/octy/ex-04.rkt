;; TODO same issue as ex-2

(define (f x)
  (if (int? x) (add1 x) (string-length x)))

(define (g x)
  (if (or (int? x) (string? x)) (f x) 0))

(provide/contract
 (f (-> (or/c string? int?) int?))
 (g (-> any? int?)))

(@unchecked f OPQ)
(@unchecked g OPQ)
(safe)
