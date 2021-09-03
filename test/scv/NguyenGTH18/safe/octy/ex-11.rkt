(define (f g p)
  (if (and (number? (car p)) (number? (cdr p))) (g p) 'no))

(provide/contract
  (f (-> (-> pair? symbol?) pair? symbol?)))

;(provide/contract
; [f (((cons/c number? number?) . -> . symbol?) cons? . -> . symbol?)])

(@unchecked f OPQ OPQ)
(safe)
