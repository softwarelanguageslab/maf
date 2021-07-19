(define (eval exp)
  (if (pair? exp)
         (leval-apply (eval (car exp)))
         '(primitive)))

(define (leval-apply procedure)
  (if (eq? (car procedure) 'primitive)
         '()
          (<change> ; <=================================================================================================
           #t
           (extend-environment
            (map cdr procedure)))))

(define (extend-environment vars)
  (list vars))

(eval '(cons))