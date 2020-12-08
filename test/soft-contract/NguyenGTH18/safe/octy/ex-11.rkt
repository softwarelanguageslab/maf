(define (f g p)
  (if (and (int? (car p)) (int? (cdr p))) (g p) 'no))

(provide/contract
  (f (-> (-> pair? symbol?) pair? symbol?)))

;(provide/contract
; [f (((cons/c int? int?) . -> . symbol?) cons? . -> . symbol?)])

(@unchecked f OPQ OPQ)
(safe)
