(define (eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (list 'primitive +))
        ((pair? exp)
         (leval-apply (eval (car exp) env)
                (cdr exp)
                env))
        (else
         (error "Unknown expression type -- EVAL"))))

(define (leval-apply procedure arguments env)
  (cond ((eq? (car procedure) 'primitive)
          (list-of-arg-values arguments env))
        ((eq? (car procedure) 'procedure)
         (eval-sequence
          '()
          (<change> ; <=================================================================================================
            (extend-environment
              (cadr procedure)
              '())
              (extend-environment
                (map cadddr procedure)
                '()))))))

(define (list-of-arg-values exps env)
  (if (null? exps)
      '()
      (cons (eval (car exps) env)
            (list-of-arg-values (cdr exps)
                                env))))

(define (eval-sequence exps env)
  (cond ((null? (cdr exps)) (eval (car exps) env))
        (else (eval (car exps) env)
              (eval-sequence (cdr exps) env))))

(define (extend-environment vars vals)
      (list (cons vars vals)))

(eval '(cons 1 2) (extend-environment (list 'cons) (list (list 'primitive cons))))