; Changes:
; * removed: 8
; * added: 7
; * swaps: 15
; * negated predicates: 2
; * swapped branches: 2
; * calls to id fun: 14
(letrec ((self-evaluating? (lambda (exp)
                             @sensitivity:FA
                             (if (<change> (number? exp) (not (number? exp)))
                                #t
                                (if (string? exp) #t #f))))
         (variable? (lambda (exp)
                      (<change>
                         @sensitivity:FA
                         ())
                      (symbol? exp)))
         (tagged-list? (lambda (exp tag)
                         @sensitivity:FA
                         (if (pair? exp) (eq? (car exp) tag) #f)))
         (quoted? (lambda (exp)
                    (<change>
                       @sensitivity:FA
                       ((lambda (x) x) @sensitivity:FA))
                    (tagged-list? exp 'quote)))
         (text-of-quotation (lambda (exp)
                              (<change>
                                 @sensitivity:FA
                                 (cadr exp))
                              (<change>
                                 (cadr exp)
                                 @sensitivity:FA)))
         (assignment? (lambda (exp)
                        @sensitivity:FA
                        (tagged-list? exp 'set!)))
         (assignment-variable (lambda (exp)
                                @sensitivity:FA
                                (<change>
                                   (cadr exp)
                                   ((lambda (x) x) (cadr exp)))))
         (assignment-value (lambda (exp)
                             (<change>
                                @sensitivity:FA
                                (caddr exp))
                             (<change>
                                (caddr exp)
                                @sensitivity:FA)))
         (definition? (lambda (exp)
                        @sensitivity:FA
                        (tagged-list? exp 'define)))
         (definition-variable (lambda (exp)
                                @sensitivity:FA
                                (if (symbol? (cadr exp)) (cadr exp) (caadr exp))))
         (make-lambda (lambda (parameters body)
                        (<change>
                           @sensitivity:FA
                           ())
                        (cons 'lambda (cons parameters body))))
         (definition-value (lambda (exp)
                             @sensitivity:FA
                             (if (symbol? (cadr exp))
                                (<change>
                                   (caddr exp)
                                   (make-lambda (cdadr exp) (cddr exp)))
                                (<change>
                                   (make-lambda (cdadr exp) (cddr exp))
                                   (caddr exp)))))
         (lambda? (lambda (exp)
                    (<change>
                       @sensitivity:FA
                       (tagged-list? exp 'lambda))
                    (<change>
                       (tagged-list? exp 'lambda)
                       @sensitivity:FA)))
         (lambda-parameters (lambda (exp)
                              (<change>
                                 ()
                                 (display exp))
                              @sensitivity:FA
                              (cadr exp)))
         (lambda-body (lambda (exp)
                        @sensitivity:FA
                        (cddr exp)))
         (if? (lambda (exp)
                @sensitivity:FA
                (tagged-list? exp 'if)))
         (if-predicate (lambda (exp)
                         (<change>
                            @sensitivity:FA
                            ())
                         (cadr exp)))
         (if-consequent (lambda (exp)
                          (<change>
                             @sensitivity:FA
                             (caddr exp))
                          (<change>
                             (caddr exp)
                             @sensitivity:FA)))
         (if-alternative (lambda (exp)
                           @sensitivity:FA
                           (<change>
                              (if (not (null? (cdddr exp)))
                                 (cadddr exp)
                                 'false)
                              ((lambda (x) x) (if (not (null? (cdddr exp))) (cadddr exp) 'false)))))
         (make-if (lambda (predicate consequent alternative)
                    (<change>
                       @sensitivity:FA
                       ())
                    (cons 'if (cons predicate (cons consequent (cons alternative ()))))))
         (begin? (lambda (exp)
                   @sensitivity:FA
                   (tagged-list? exp 'begin)))
         (begin-actions (lambda (exp)
                          (<change>
                             @sensitivity:FA
                             ((lambda (x) x) @sensitivity:FA))
                          (cdr exp)))
         (last-exp? (lambda (seq)
                      (<change>
                         @sensitivity:FA
                         ((lambda (x) x) @sensitivity:FA))
                      (null? (cdr seq))))
         (first-exp (lambda (seq)
                      @sensitivity:FA
                      (<change>
                         (car seq)
                         ((lambda (x) x) (car seq)))))
         (rest-exps (lambda (seq)
                      @sensitivity:FA
                      (<change>
                         ()
                         (cdr seq))
                      (cdr seq)))
         (mk-begin (lambda (seq)
                     @sensitivity:FA
                     (cons 'begin seq)))
         (sequence->exp (lambda (seq)
                          @sensitivity:FA
                          (if (null? seq)
                             seq
                             (if (last-exp? seq)
                                (first-exp seq)
                                (mk-begin seq)))))
         (application? (lambda (exp)
                         (<change>
                            @sensitivity:FA
                            (pair? exp))
                         (<change>
                            (pair? exp)
                            @sensitivity:FA)))
         (operator (lambda (exp)
                     @sensitivity:FA
                     (car exp)))
         (operands (lambda (exp)
                     @sensitivity:FA
                     (cdr exp)))
         (no-operands? (lambda (ops)
                         @sensitivity:FA
                         (null? ops)))
         (first-operand (lambda (ops)
                          (<change>
                             @sensitivity:FA
                             ((lambda (x) x) @sensitivity:FA))
                          (car ops)))
         (rest-operands (lambda (ops)
                          @sensitivity:FA
                          (cdr ops)))
         (cond? (lambda (exp)
                  (<change>
                     @sensitivity:FA
                     ((lambda (x) x) @sensitivity:FA))
                  (<change>
                     (tagged-list? exp 'cond)
                     ((lambda (x) x) (tagged-list? exp 'cond)))))
         (cond-clauses (lambda (exp)
                         (<change>
                            @sensitivity:FA
                            (cdr exp))
                         (<change>
                            (cdr exp)
                            @sensitivity:FA)))
         (cond-predicate (lambda (clause)
                           @sensitivity:FA
                           (car clause)))
         (cond-else-clause? (lambda (clause)
                              (<change>
                                 @sensitivity:FA
                                 (eq? (cond-predicate clause) 'else))
                              (<change>
                                 (eq? (cond-predicate clause) 'else)
                                 @sensitivity:FA)))
         (cond-actions (lambda (clause)
                         (<change>
                            ()
                            cdr)
                         @sensitivity:FA
                         (<change>
                            ()
                            (display @sensitivity:FA))
                         (cdr clause)))
         (expand-clauses (lambda (clauses)
                           @sensitivity:No
                           (if (null? clauses)
                              (<change>
                                 'false
                                 (let ((first (car clauses))
                                       (rest (cdr clauses)))
                                    (if (cond-else-clause? first)
                                       (if (null? rest)
                                          (sequence->exp (cond-actions first))
                                          (error "ELSE clause isn't last -- COND->IF" clauses))
                                       (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))
                              (<change>
                                 (let ((first (car clauses))
                                       (rest (cdr clauses)))
                                    (if (cond-else-clause? first)
                                       (if (null? rest)
                                          (sequence->exp (cond-actions first))
                                          (error "ELSE clause isn't last -- COND->IF" clauses))
                                       (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest))))
                                 'false))))
         (cond->if (lambda (exp)
                     @sensitivity:FA
                     (<change>
                        (expand-clauses (cond-clauses exp))
                        ((lambda (x) x) (expand-clauses (cond-clauses exp))))))
         (true? (lambda (x)
                  @sensitivity:FA
                  (<change>
                     (not (eq? x #f))
                     ((lambda (x) x) (not (eq? x #f))))))
         (false? (lambda (x)
                   (<change>
                      @sensitivity:FA
                      ((lambda (x) x) @sensitivity:FA))
                   (eq? x #f)))
         (make-procedure (lambda (parameters body env)
                           @sensitivity:FA
                           (cons 'procedure (cons parameters (cons body (cons env ()))))))
         (compound-procedure? (lambda (p)
                                @sensitivity:FA
                                (tagged-list? p 'procedure)))
         (procedure-parameters (lambda (p)
                                 (<change>
                                    @sensitivity:FA
                                    ())
                                 (cadr p)))
         (procedure-body (lambda (p)
                           @sensitivity:FA
                           (<change>
                              ()
                              (display caddr))
                           (caddr p)))
         (procedure-environment (lambda (p)
                                  @sensitivity:FA
                                  (<change>
                                     ()
                                     cadddr)
                                  (cadddr p)))
         (enclosing-environment (lambda (env)
                                  @sensitivity:FA
                                  (cdr env)))
         (first-frame (lambda (env)
                        @sensitivity:FA
                        (car env)))
         (the-empty-environment ())
         (make-frame (lambda (variables values)
                       (<change>
                          @sensitivity:FA
                          (cons variables values))
                       (<change>
                          (cons variables values)
                          @sensitivity:FA)))
         (frame-variables (lambda (frame)
                            @sensitivity:FA
                            (car frame)))
         (frame-values (lambda (frame)
                         @sensitivity:FA
                         (cdr frame)))
         (add-binding-to-frame! (lambda (var val frame)
                                  @sensitivity:FA
                                  (set-car! frame (cons var (car frame)))
                                  (set-cdr! frame (cons val (cdr frame)))))
         (extend-environment (lambda (vars vals base-env)
                               (<change>
                                  @sensitivity:No
                                  (if (= (length vars) (length vals))
                                     (cons (make-frame vars vals) base-env)
                                     (if (< (length vars) (length vals))
                                        (error "Too many arguments supplied" vars vals)
                                        (error "Too few arguments supplied" vars vals))))
                               (<change>
                                  (if (= (length vars) (length vals))
                                     (cons (make-frame vars vals) base-env)
                                     (if (< (length vars) (length vals))
                                        (error "Too many arguments supplied" vars vals)
                                        (error "Too few arguments supplied" vars vals)))
                                  @sensitivity:No)))
         (lookup-variable-value (lambda (var env)
                                  (<change>
                                     @sensitivity:FA
                                     ())
                                  (letrec ((env-loop (lambda (env)
                                                       (<change>
                                                          @sensitivity:FA
                                                          ((lambda (x) x) @sensitivity:FA))
                                                       (letrec ((scan (lambda (vars vals)
                                                                        @sensitivity:FA
                                                                        (if (null? vars)
                                                                           (env-loop (enclosing-environment env))
                                                                           (if (eq? var (car vars))
                                                                              (car vals)
                                                                              (scan (cdr vars) (cdr vals)))))))
                                                          (if (eq? env the-empty-environment)
                                                             (error "Unbound variable" var)
                                                             (let ((frame (first-frame env)))
                                                                (scan (frame-variables frame) (frame-values frame))))))))
                                     (env-loop env))))
         (set-variable-value! (lambda (var val env)
                                @sensitivity:FA
                                (letrec ((env-loop (lambda (env)
                                                     @sensitivity:FA
                                                     (letrec ((scan (lambda (vars vals)
                                                                      @sensitivity:FA
                                                                      (if (null? vars)
                                                                         (env-loop (enclosing-environment env))
                                                                         (if (eq? var (car vars))
                                                                            (set-car! vals val)
                                                                            (scan (cdr vars) (cdr vals)))))))
                                                        (if (eq? env the-empty-environment)
                                                           (error "Unbound variable -- SET!" var)
                                                           (let ((frame (first-frame env)))
                                                              (scan (frame-variables frame) (frame-values frame))))))))
                                   (env-loop env))))
         (define-variable! (lambda (var val env)
                             (<change>
                                @sensitivity:FA
                                ())
                             (let ((frame (first-frame env)))
                                (letrec ((scan (lambda (vars vals)
                                                 @sensitivity:FA
                                                 (if (null? vars)
                                                    (add-binding-to-frame! var val frame)
                                                    (if (eq? var (car vars))
                                                       (set-car! vals val)
                                                       (scan (cdr vars) (cdr vals)))))))
                                   (scan (frame-variables frame) (frame-values frame))))))
         (primitive-procedure? (lambda (proc)
                                 @sensitivity:FA
                                 (tagged-list? proc 'primitive)))
         (primitive-implementation (lambda (proc)
                                     @sensitivity:FA
                                     (cadr proc)))
         (primitive-procedures (cons (cons '= (cons = ())) (cons (cons '* (cons * ())) (cons (cons '- (cons - ())) ()))))
         (primitive-procedure-names (lambda ()
                                      @sensitivity:FA
                                      (map car primitive-procedures)))
         (primitive-procedure-objects (lambda ()
                                        (<change>
                                           @sensitivity:FA
                                           (map (lambda (proc) (cons 'primitive (cons (cadr proc) ()))) primitive-procedures))
                                        (<change>
                                           (map (lambda (proc) (cons 'primitive (cons (cadr proc) ()))) primitive-procedures)
                                           @sensitivity:FA)))
         (setup-environment (lambda ()
                              (let ((initial-env (extend-environment
                                                   (primitive-procedure-names)
                                                   (primitive-procedure-objects)
                                                   the-empty-environment)))
                                 (<change>
                                    (define-variable! 'true #t initial-env)
                                    (define-variable! 'false #f initial-env))
                                 (<change>
                                    (define-variable! 'false #f initial-env)
                                    (define-variable! 'true #t initial-env))
                                 (<change>
                                    initial-env
                                    ((lambda (x) x) initial-env)))))
         (the-global-environment (setup-environment))
         (apply-primitive-procedure (lambda (proc args)
                                      @sensitivity:FA
                                      (let ((f (primitive-implementation proc))
                                            (n (length args)))
                                         (if (= n 0)
                                            (f)
                                            (if (= n 1)
                                               (f (car args))
                                               (if (= n 2)
                                                  (f (car args) (cadr args))
                                                  (error "ERROR -- can't handle more than two arguments")))))))
         (mceval (lambda (exp env)
                   @sensitivity:FA
                   (letrec ((eval-sequence (lambda (exps env)
                                             @sensitivity:FA
                                             (if (last-exp? exps)
                                                (mceval (first-exp exps) env)
                                                (begin
                                                   (<change>
                                                      (mceval (first-exp exps) env)
                                                      (eval-sequence (rest-exps exps) env))
                                                   (<change>
                                                      (eval-sequence (rest-exps exps) env)
                                                      (mceval (first-exp exps) env)))))))
                      (let ((mcapply (lambda (procedure arguments)
                                       (<change>
                                          @sensitivity:FA
                                          ())
                                       (if (primitive-procedure? procedure)
                                          (apply-primitive-procedure procedure arguments)
                                          (if (compound-procedure? procedure)
                                             (eval-sequence
                                                (procedure-body procedure)
                                                (extend-environment (procedure-parameters procedure) arguments (procedure-environment procedure)))
                                             (error "Unknown procedure type -- APPLY" procedure))))))
                         (let ((eval-if (lambda (exp env)
                                          (<change>
                                             @sensitivity:FA
                                             (if (not (true? (mceval (if-predicate exp) env)))
                                                (mceval (if-consequent exp) env)
                                                (mceval (if-alternative exp) env)))
                                          (<change>
                                             (if (true? (mceval (if-predicate exp) env))
                                                (mceval (if-consequent exp) env)
                                                (mceval (if-alternative exp) env))
                                             @sensitivity:FA))))
                            (let ((eval-assignment (lambda (exp env)
                                                     (<change>
                                                        ()
                                                        env)
                                                     (<change>
                                                        (set-variable-value! (assignment-variable exp) (mceval (assignment-value exp) env) env)
                                                        'ok)
                                                     (<change>
                                                        'ok
                                                        (set-variable-value! (assignment-variable exp) (mceval (assignment-value exp) env) env)))))
                               (let ((eval-definition (lambda (exp env)
                                                        (<change>
                                                           @sensitivity:FA
                                                           (define-variable! (definition-variable exp) (mceval (definition-value exp) env) env))
                                                        (<change>
                                                           (define-variable! (definition-variable exp) (mceval (definition-value exp) env) env)
                                                           @sensitivity:FA)
                                                        'ok)))
                                  (letrec ((list-of-values (lambda (exps env)
                                                             @sensitivity:FA
                                                             (if (no-operands? exps)
                                                                ()
                                                                (cons (mceval (first-operand exps) env) (list-of-values (rest-operands exps) env))))))
                                     (if (self-evaluating? exp)
                                        exp
                                        (if (variable? exp)
                                           (lookup-variable-value exp env)
                                           (if (quoted? exp)
                                              (text-of-quotation exp)
                                              (if (assignment? exp)
                                                 (eval-assignment exp env)
                                                 (if (definition? exp)
                                                    (eval-definition exp env)
                                                    (if (if? exp)
                                                       (eval-if exp env)
                                                       (if (lambda? exp)
                                                          (make-procedure (lambda-parameters exp) (lambda-body exp) env)
                                                          (if (begin? exp)
                                                             (eval-sequence (begin-actions exp) env)
                                                             (if (cond? exp)
                                                                (mceval (cond->if exp) env)
                                                                (if (application? exp)
                                                                   (mcapply (mceval (operator exp) env) (list-of-values (operands exp) env))
                                                                   (error "Unknown expression type -- EVAL" exp))))))))))))))))))))
   (mceval
      (__toplevel_cons
         (__toplevel_cons
            (__toplevel_cons
               'lambda
               (__toplevel_cons
                  (__toplevel_cons 'f ())
                  (__toplevel_cons
                     (__toplevel_cons
                        'lambda
                        (__toplevel_cons
                           (__toplevel_cons 'x ())
                           (__toplevel_cons (__toplevel_cons 'f (__toplevel_cons 'f (__toplevel_cons 'x ()))) ())))
                     ())))
            (__toplevel_cons
               (__toplevel_cons
                  'lambda
                  (__toplevel_cons
                     (__toplevel_cons 'g (__toplevel_cons 'n ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           'if
                           (__toplevel_cons
                              (__toplevel_cons '= (__toplevel_cons 'n (__toplevel_cons 0 ())))
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       '*
                                       (__toplevel_cons
                                          'n
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'g
                                                (__toplevel_cons
                                                   'g
                                                   (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'n (__toplevel_cons 1 ()))) ())))
                                             ())))
                                    ()))))
                        ())))
               ()))
         (__toplevel_cons 8 ()))
      the-global-environment))