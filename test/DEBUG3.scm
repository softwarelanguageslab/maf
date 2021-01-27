(define (translate-term term)
  (cond ((not (pair? term))
         term)
        (else (cons (symbol->symbol-record (car term))
                    (translate-term (cdr term))))))

(define (symbol->symbol-record sym)
  (let ((x (assq sym *symbol-records-alist*)))
    (if x
        (cdr x)
        (begin (set! *symbol-records-alist* (list (cons sym (list sym))))
               (list sym)))))

(define *symbol-records-alist* '())

(define (rewrite term)
   (rewrite-with-lemmas term (cdr (car term))))

(define (rewrite-with-lemmas term lst)
  (one-way-unify1 term (car (cdr (car lst))))
  (rewrite (caddr (car lst))))

(define unify-subst '*)

(define (one-way-unify1 term1 term2)
  (if (not (pair? term2))
      (let ((temp (assq term2 unify-subst)))
        (<change> #t #f) ; <<==============================================
        (set! unify-subst (cons (cons term2 term1)
                                unify-subst)))))

(rewrite (translate-term  (list (quote (implies (implies u w) (implies x w))))))