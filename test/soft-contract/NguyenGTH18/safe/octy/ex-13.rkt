(define (f x y)
  (cond
   ((and (int? x) (string? y)) (and (int? x) (string? y)))
   ((int? x) (and (int? x) (not (string? y))))
   (else (not (int? x)))))

(provide/contract 
  (f (-> any/c any/c (not/c false?))))

(@unchecked f OPQ OPQ)
(safe)
