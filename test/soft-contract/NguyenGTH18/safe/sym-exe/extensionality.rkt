;; 1421365820240

(define (f g)
  (= (g 5) (g 5)))

(provide/contract
    (f (-> (-> int? int?) (lambda (x) x))))

(@unchecked f OPQ)
(safe)
