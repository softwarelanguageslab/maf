(define (f p)
  (if (int? (car p)) (add1 (car p)) 7))

(provide/contract
  (f (-> pair? int?)))

; (provide/contract [f (cons? . -> . int?)])
(@unchecked f OPQ)
(safe)
