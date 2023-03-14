(define (f input extra)
  (cond
   ((and (number? input) (number? (car extra)))
    (+ input (car extra)))
   ((number? (car extra))
    (+ (string-length input) (car extra)))
   (else 0)))

(provide/contract
 (f (-> (or/c number? string?) pair? number?)))

(@unchecked f OPQ OPQ)
(safe)
