;; OK

(define (f x y)
  (if (if (int? x) (string? y) #f)
      (+ x (string-length y))
      0))

(provide/contract 
  (f (-> any? any? int?)))

(@unchecked f OPQ OPQ)
(safe)
