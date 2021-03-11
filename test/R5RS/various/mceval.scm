(define (self-evaluating? exp)
  @sensitivity:FA
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))
(define (variable? exp)
  @sensitivity:FA
  (symbol? exp))
(define (tagged-list? exp tag)
  @sensitivity:FA
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
(define (quoted? exp)
  @sensitivity:FA
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  @sensitivity:FA
  (cadr exp))
(define (assignment? exp)
  @sensitivity:FA
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  @sensitivity:FA
  (cadr exp))
(define (assignment-value exp)
  @sensitivity:FA
  (caddr exp))
(define (definition? exp)
  @sensitivity:FA
  (tagged-list? exp 'define))
(define (definition-variable exp)
  @sensitivity:FA
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (make-lambda parameters body)
  @sensitivity:FA
  (cons 'lambda (cons parameters body)))
(define (definition-value exp)
  @sensitivity:FA
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
(define (lambda? exp)
  @sensitivity:FA
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  @sensitivity:FA
  (cadr exp))
(define (lambda-body exp)
  @sensitivity:FA
  (cddr exp))
(define (if? exp)
  @sensitivity:FA
  (tagged-list? exp 'if))
(define (if-predicate exp)
  @sensitivity:FA
  (cadr exp))
(define (if-consequent exp)
  @sensitivity:FA
  (caddr exp))
(define (if-alternative exp)
  @sensitivity:FA
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  @sensitivity:FA
  (cons 'if (cons predicate (cons consequent (cons alternative '())))))
(define (begin? exp)
  @sensitivity:FA
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  @sensitivity:FA
  (cdr exp))
(define (last-exp? seq)
  @sensitivity:FA
  (null? (cdr seq)))
(define (first-exp seq) @sensitivity:FA (car seq))
(define (rest-exps seq) @sensitivity:FA (cdr seq))
(define (mk-begin seq) @sensitivity:FA (cons 'begin seq))
(define (sequence->exp seq)
  @sensitivity:FA
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (mk-begin seq))))
(define (application? exp) @sensitivity:FA (pair? exp))
(define (operator exp) @sensitivity:FA (car exp))
(define (operands exp) @sensitivity:FA (cdr exp))
(define (no-operands? ops) @sensitivity:FA (null? ops))
(define (first-operand ops) @sensitivity:FA (car ops))
(define (rest-operands ops) @sensitivity:FA (cdr ops))
(define (cond? exp) @sensitivity:FA (tagged-list? exp 'cond))
(define (cond-clauses exp) @sensitivity:FA (cdr exp))
(define (cond-predicate clause) @sensitivity:FA (car clause))
(define (cond-else-clause? clause) @sensitivity:FA (eq? (cond-predicate clause) 'else))
(define (cond-actions clause) @sensitivity:FA (cdr clause))
(define (expand-clauses clauses)
  @sensitivity:No
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
(define (cond->if exp) @sensitivity:FA (expand-clauses (cond-clauses exp)))
(define (true? x) @sensitivity:FA (not (eq? x #f)))
(define (false? x) @sensitivity:FA  (eq? x #f))
(define (make-procedure parameters body env) @sensitivity:FA (cons 'procedure (cons parameters (cons body (cons env '())))))
(define (compound-procedure? p) @sensitivity:FA (tagged-list? p 'procedure))
(define (procedure-parameters p) @sensitivity:FA (cadr p))
(define (procedure-body p) @sensitivity:FA (caddr p))
(define (procedure-environment p) @sensitivity:FA  (cadddr p))
(define (enclosing-environment env) @sensitivity:FA  (cdr env))
(define (first-frame env) @sensitivity:FA  (car env))
(define the-empty-environment '())
(define (make-frame variables values) @sensitivity:FA (cons variables values))
(define (frame-variables frame) @sensitivity:FA (car frame))
(define (frame-values frame) @sensitivity:FA (cdr frame))
(define (add-binding-to-frame! var val frame)
  @sensitivity:FA
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  @sensitivity:No
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  @sensitivity:FA
  (letrec ((env-loop (lambda (env)
                       @sensitivity:FA
                       (letrec ((scan (lambda (vars vals)
                                        @sensitivity:FA
                                        (cond ((null? vars)
                                               (env-loop (enclosing-environment env)))
                                              ((eq? var (car vars))
                                               (car vals))
                                              (else (scan (cdr vars) (cdr vals)))))))
                         (if (eq? env the-empty-environment)
                             (error "Unbound variable" var)
                             (let ((frame (first-frame env)))
                               (scan (frame-variables frame)
                                     (frame-values frame))))))))
    (env-loop env)))
(define (set-variable-value! var val env)
  @sensitivity:FA
  (letrec ((env-loop (lambda (env)
                       @sensitivity:FA
                       (letrec ((scan (lambda (vars vals)
                                        @sensitivity:FA
                                        (cond ((null? vars)
                                               (env-loop (enclosing-environment env)))
                                              ((eq? var (car vars))
                                               (set-car! vals val))
                                              (else (scan (cdr vars) (cdr vals)))))))
                         (if (eq? env the-empty-environment)
                             (error "Unbound variable -- SET!" var)
                             (let ((frame (first-frame env)))
                               (scan (frame-variables frame)
                                     (frame-values frame))))))))
    (env-loop env)))
(define (define-variable! var val env)
  @sensitivity:FA
  (let ((frame (first-frame env)))
    (letrec ((scan (lambda (vars vals)
                     @sensitivity:FA
                     (cond ((null? vars)
                            (add-binding-to-frame! var val frame))
                           ((eq? var (car vars))
                            (set-car! vals val))
                           (else (scan (cdr vars) (cdr vals)))))))
      (scan (frame-variables frame)
            (frame-values frame)))))
(define (primitive-procedure? proc) @sensitivity:FA (tagged-list? proc 'primitive))
(define (primitive-implementation proc) @sensitivity:FA (cadr proc))
(define primitive-procedures
  (cons (cons '= (cons = '()))
        (cons (cons '* (cons * '()))
              (cons (cons '- (cons - '())) '()))))
(define (primitive-procedure-names)
  @sensitivity:FA
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  @sensitivity:FA
  (map (lambda (proc) (cons 'primitive (cons (cadr proc) '())))
       primitive-procedures))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define (apply-primitive-procedure proc args)
  @sensitivity:FA
  (let ((f (primitive-implementation proc))
        (n (length args)))
    (cond ((= n 0) (f))
          ((= n 1) (f (car args)))
          ((= n 2) (f (car args) (cadr args)))
          (else (error "ERROR -- can't handle more than two arguments")))))
(define (mceval exp env)
  @sensitivity:FA
  (letrec ((eval-sequence (lambda (exps env)
                            @sensitivity:FA
                            (cond ((last-exp? exps) (mceval (first-exp exps) env))
                                  (else (mceval (first-exp exps) env)
                                        (eval-sequence (rest-exps exps) env))))))
    (let ((mcapply (lambda (procedure arguments)
                     @sensitivity:FA
                     (cond ((primitive-procedure? procedure)
                            (apply-primitive-procedure procedure arguments))
                           ((compound-procedure? procedure)
                            (eval-sequence
                             (procedure-body procedure)
                             (extend-environment
                              (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
                           (else
                            (error
                             "Unknown procedure type -- APPLY" procedure))))))
      (let ((eval-if (lambda (exp env)
                       @sensitivity:FA
                       (if (true? (mceval (if-predicate exp) env))
                           (mceval (if-consequent exp) env)
                           (mceval (if-alternative exp) env)))))
        (let ((eval-assignment (lambda (exp env)
                                 (set-variable-value! (assignment-variable exp)
                                                      (mceval (assignment-value exp) env)
                                                      env)
                                 'ok)))
          (let ((eval-definition (lambda (exp env)
                                   @sensitivity:FA
                                   (define-variable! (definition-variable exp)
                                     (mceval (definition-value exp) env)
                                     env)
                                   'ok)))
            (letrec ((list-of-values (lambda (exps env)
                                       @sensitivity:FA
                                       (if (no-operands? exps)
                                           '()
                                           (cons (mceval (first-operand exps) env)
                                                 (list-of-values (rest-operands exps) env))))))
              (cond ((self-evaluating? exp) exp)
                    ((variable? exp) (lookup-variable-value exp env))
                    ((quoted? exp) (text-of-quotation exp))
                    ((assignment? exp) (eval-assignment exp env))
                    ((definition? exp) (eval-definition exp env))
                    ((if? exp) (eval-if exp env))
                    ((lambda? exp)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env))
                    ((begin? exp)
                     (eval-sequence (begin-actions exp) env))
                    ((cond? exp) (mceval (cond->if exp) env))
                    ((application? exp)
                     (mcapply (mceval (operator exp) env)
                              (list-of-values (operands exp) env)))
                    (else
                     (error "Unknown expression type -- EVAL" exp))))))))))
(mceval '(((lambda (f) (lambda (x) (f f x)))
           (lambda (g n)
             (if (= n 0)
                 1
                 (* n (g g (- n 1)))))) 8)
        the-global-environment)
