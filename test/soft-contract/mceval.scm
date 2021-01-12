
(define @sensitivity:FA 'nil)
(define @sensitivity:No 'nil)

(define/contract (self-evaluating? exp)
  (-> any? bool?)
  @sensitivity:FA
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))
(define/contract (variable? exp)
  (-> any? bool?)
  @sensitivity:FA
  (symbol? exp))
(define/contract (tagged-list? exp tag)
  (-> any? bool?)
  @sensitivity:FA
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
(define/contract (quoted? exp)
  (-> any? bool?)
  @sensitivity:FA
  (tagged-list? exp 'quote))

(define/contract (text-of-quotation exp)
  (-> quoted? any?)
  @sensitivity:FA
  (cadr exp))

(define/contract (assignment? exp)
  (-> any? bool?)
  @sensitivity:FA
  (tagged-list? exp 'set!))

(define/contract (assignment-variable exp)
  (-> assignment? symbol?)
  @sensitivity:FA
  (cadr exp))

(define/contract (assignment-value exp)
  (-> assignment? any?)
  @sensitivity:FA
  (caddr exp))

(define/contract (definition? exp)
  (-> any? bool?)
  @sensitivity:FA
  (tagged-list? exp 'define))

(define/contract (definition-variable exp)
  (-> definition? symbol?)
  @sensitivity:FA
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define/contract (make-lambda parameters body)
  (-> any? list? (lambda (exp) (tagged-list? exp 'lambda)))
  @sensitivity:FA
  (cons 'lambda (cons parameters body)))

(define/contract (definition-value exp)
  (-> definition? any?)
  @sensitivity:FA
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define/contract (lambda? exp)
  (-> any? bool?)
  @sensitivity:FA
  (tagged-list? exp 'lambda))

(define/contract (lambda-parameters exp)
  (-> lambda? pair?)
  @sensitivity:FA
  (cadr exp))

(define/contract (lambda-body exp)
  (-> lambda? list?)
  @sensitivity:FA
  (cddr exp))

(define/contract (if? exp)
  (-> any? bool?)
  @sensitivity:FA
  (tagged-list? exp 'if))

(define/contract (if-predicate exp)
  (-> if? any?)
  @sensitivity:FA
  (cadr exp))

(define/contract (if-consequent exp)
  (-> if? any?)
  @sensitivity:FA
  (caddr exp))
(define/contract (if-alternative exp)
  (-> if? any?)
  @sensitivity:FA
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define/contract (make-if predicate consequent alternative)
  (-> any? any? any? if?)
  @sensitivity:FA
  (cons 'if (cons predicate (cons consequent (cons alternative '())))))

(define/contract (begin? exp)
  (-> any? bool?)
  @sensitivity:FA
  (tagged-list? exp 'begin))

(define/contract (begin-actions exp)
  (-> begin? list?)
  @sensitivity:FA
  (cdr exp))

(define/contract (last-exp? seq)
  (-> list? bool?)
  @sensitivity:FA
  (null? (cdr seq)))

(define/contract (first-exp seq) 
   (-> pair? any?)
   @sensitivity:FA (car seq))

(define/contract (rest-exps seq) 
   (-> pair? (or/c null? pair?))
   @sensitivity:FA 
   (cdr seq))

(define/contract (mk-begin seq)
   (-> list? begin?)
   @sensitivity:FA (cons 'begin seq))

(define/contract (sequence->exp seq)
  (-> list? begin?)
  @sensitivity:FA
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (mk-begin seq))))

(define/contract (application? exp) 
   (-> any? bool?)
   @sensitivity:FA (pair? exp))

(define/contract (operator exp) 
   (-> application? any?)
   @sensitivity:FA (car exp))

(define/contract (operands exp) 
   (-> application? any?)
   @sensitivity:FA (cdr exp))

(define/contract (no-operands? ops) 
   (-> any? bool?)
   @sensitivity:FA (null? ops))

(define/contract (first-operand ops)
   (-> pair? any?)
   @sensitivity:FA (car ops))

(define/contract (rest-operands ops)
                 (-> pair? any?)
                 @sensitivity:FA 
                 (cdr ops))

(define/contract (cond? exp) 
  (-> any? bool?)
  @sensitivity:FA (tagged-list? exp 'cond))

(define/contract (cond-clauses exp) 
  (-> cond? list?)
  @sensitivity:FA (cdr exp))

(define/contract (cond-predicate clause) 
   (-> pair? any?)
   @sensitivity:FA (car clause))

(define/contract (cond-else-clause? clause) 
                 (-> pair? bool?)
                 @sensitivity:FA (eq? (cond-predicate clause) 'else))

(define/contract (cond-actions clause) 
                 (-> pair? bool?)
                 @sensitivity:FA (cdr clause))
(define/contract (expand-clauses clauses)
  (-> any? any?)
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
(define/contract (true? x) 
                 (-> any? bool?)
                 @sensitivity:FA (not (eq? x #f)))

(define/contract (false? x) 
                 (-> any? bool?)
                 @sensitivity:FA  (eq? x #f))

(define/contract (compound-procedure? p) 
                 (-> any? bool?)
                 @sensitivity:FA (tagged-list? p 'procedure))

(define/contract (make-procedure parameters body env) 
                 (-> any? any? any? compound-procedure?)
                 @sensitivity:FA (cons 'procedure (cons parameters (cons body (cons env '())))))

(define/contract (procedure-parameters p) 
                 (-> compound-procedure? (or/c pair? null?))
                 @sensitivity:FA (cadr p))

(define/contract (procedure-body p) 
                 (-> compound-procedure? any?)
                 @sensitivity:FA (caddr p))

(define/contract (procedure-environment p) 
                 (-> compound-procedure? any?)
                 @sensitivity:FA  (cadddr p))

(define/contract (enclosing-environment env) 
                 (-> pair? (or/c pair? null?))
                 @sensitivity:FA  (cdr env))

(define/contract (first-frame env) 
                 (-> pair? any?)
                 @sensitivity:FA  (car env))

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
(define/contract (lookup-variable-value var env)
  (-> symbol? pair? any?)
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
(define/contract (set-variable-value! var val env)
  (-> symbol? any? pair? any?)
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

(define/contract (define-variable! var val env)
  (-> symbol? any? pair? any?)
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
(define/contract 
  (primitive-procedure? proc) 
  (-> any? bool?)
  @sensitivity:FA 
  (tagged-list? proc 'primitive))

(define/contract (primitive-implementation proc) 
                 (-> primitive-procedure? proc?)
                 @sensitivity:FA (cadr proc))

(define primitive-procedures
  (cons (cons '= (cons = '()))
        (cons (cons '* (cons * '()))
              (cons (cons '- (cons - '())) '()))))

(define/contract (primitive-procedure-names)
  (-> (list-of symbol?))
  @sensitivity:FA
  (map car
       primitive-procedures))

(define/contract (primitive-procedure-objects)
  (-> (list-of proc?))
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
