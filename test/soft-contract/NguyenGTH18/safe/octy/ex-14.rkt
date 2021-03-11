(define (f input extra)
  (cond
   ((and (int? input) (int? (car extra)))
    (+ input (car extra)))
   ((int? (car extra))
    (+ (string-length input) (car extra)))
   (else 0)))

(provide/contract
 (f (-> (or/c int? string?) pair? int?)))

(@unchecked f OPQ OPQ)
(safe)
