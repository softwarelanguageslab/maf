(define (translate-term term)
  (if (not (pair? term))
         term
        (cons (symbol->symbol-record (car term))
                    (translate-term (cdr term)))))

(define (symbol->symbol-record sym)
  (let ((x (assq sym *symbol-records-alist*)))
    (if x
        (cdr x)
        (begin (set! *symbol-records-alist* (list (list sym sym)))
               (list sym)))))

(define *symbol-records-alist* '())

(define (rewrite term)
   (rewrite-with-lemmas term (cdr (car term))))

(define (rewrite-with-lemmas term lst)
  (one-way-unify1 term (car (car lst)))
  (rewrite-with-lemmas (car lst) '()))

(define unify-subst '*)

(define (one-way-unify1 term1 term2)
  (if (not (pair? term2))
      (begin
        (assq term2 unify-subst)
        (<change> #t #f) ; <<==============================================
        (set! unify-subst (cons (cons term1 term1)
                                unify-subst)))))

(rewrite (translate-term (list '(implies (implies x w)))))