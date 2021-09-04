(define (subst* new old t)
  (cond
   ((equal? old t) new)
   ((pair? t) (cons (subst* new old (car t))
		    (subst* new old (cdr t))))
   (else t)))

(provide/contract 
  (subst* (-> any? any? any? any?)))

(@unchecked subst* OPQ OPQ OPQ)
(safe)
