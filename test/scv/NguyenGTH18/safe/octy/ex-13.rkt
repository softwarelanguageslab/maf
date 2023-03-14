(define (f x y)
  (cond
   ((and (number? x) (string? y)) (and (number? x) (string? y)))
   ((number? x) (and (number? x) (not (string? y))))
   (else (not (number? x)))))

(provide/contract 
  (f (-> any/c any/c (not/c false?))))

(@unchecked f OPQ OPQ)
(safe)
