; Changes:
; * removed: 10
; * added: 3
; * swaps: 12
; * negated predicates: 2
; * swapped branches: 1
; * calls to id fun: 10
(letrec ((self-evaluating? (lambda (exp)
                             @sensitivity:FA
                             (<change>
                                (if (number? exp) #t (if (string? exp) #t #f))
                                ((lambda (x) x) (if (<change> (number? exp) (not (number? exp))) #t (if (string? exp) #t #f))))))
         (variable? (lambda (exp)
                      @sensitivity:FA
                      (symbol? exp)))
         (tagged-list? (lambda (exp tag)
                         @sensitivity:FA
                         (if (pair? exp) (eq? (car exp) tag) #f)))
         (quoted? (lambda (exp)
                    @sensitivity:FA
                    (tagged-list? exp 'quote)))
         (text-of-quotation (lambda (exp)
                              @sensitivity:FA
                              (<change>
                                 (cadr exp)
                                 ((lambda (x) x) (cadr exp)))))
         (assignment? (lambda (exp)
                        @sensitivity:FA
                        (tagged-list? exp 'set!)))
         (assignment-variable (lambda (exp)
                                (<change>
                                   @sensitivity:FA
                                   ((lambda (x) x) @sensitivity:FA))
                                (cadr exp)))
         (assignment-value (lambda (exp)
                             (<change>
                                @sensitivity:FA
                                (caddr exp))
                             (<change>
                                (caddr exp)
                                @sensitivity:FA)))
         (definition? (lambda (exp)
                        (<change>
                           @sensitivity:FA
                           ())
                        (tagged-list? exp 'define)))
         (definition-variable (lambda (exp)
                                @sensitivity:FA
                                (if (symbol? (cadr exp)) (cadr exp) (caadr exp))))
         (make-lambda (lambda (parameters body)
                        @sensitivity:FA
                        (cons 'lambda (cons parameters body))))
         (definition-value (lambda (exp)
                             (<change>
                                @sensitivity:FA
                                ())
                             (if (symbol? (cadr exp))
                                (caddr exp)
                                (make-lambda (cdadr exp) (cddr exp)))))
         (lambda? (lambda (exp)
                    (<change>
                       @sensitivity:FA
                       (tagged-list? exp 'lambda))
                    (<change>
                       (tagged-list? exp 'lambda)
                       @sensitivity:FA)))
         (lambda-parameters (lambda (exp)
                              @sensitivity:FA
                              (cadr exp)))
         (lambda-body (lambda (exp)
                        @sensitivity:FA
                        (cddr exp)))
         (if? (lambda (exp)
                (<change>
                   @sensitivity:FA
                   (tagged-list? exp 'if))
                (<change>
                   (tagged-list? exp 'if)
                   @sensitivity:FA)))
         (if-predicate (lambda (exp)
                         @sensitivity:FA
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
                           (if (not (null? (cdddr exp)))
                              (cadddr exp)
                              'false)))
         (make-if (lambda (predicate consequent alternative)
                    @sensitivity:FA
                    (<change>
                       ()
                       (cons 'if (cons predicate (cons consequent (cons alternative ())))))
                    (cons 'if (cons predicate (cons consequent (cons alternative ()))))))
         (begin? (lambda (exp)
                   @sensitivity:FA
                   (tagged-list? exp 'begin)))
         (begin-actions (lambda (exp)
                          (<change>
                             ()
                             exp)
                          @sensitivity:FA
                          (cdr exp)))
         (last-exp? (lambda (seq)
                      (<change>
                         @sensitivity:FA
                         (null? (cdr seq)))
                      (<change>
                         (null? (cdr seq))
                         @sensitivity:FA)))
         (first-exp (lambda (seq)
                      @sensitivity:FA
                      (car seq)))
         (rest-exps (lambda (seq)
                      @sensitivity:FA
                      (cdr seq)))
         (mk-begin (lambda (seq)
                     (<change>
                        @sensitivity:FA
                        (cons 'begin seq))
                     (<change>
                        (cons 'begin seq)
                        @sensitivity:FA)))
         (sequence->exp (lambda (seq)
                          (<change>
                             @sensitivity:FA
                             (if (null? seq)
                                seq
                                (if (last-exp? seq)
                                   (first-exp seq)
                                   (mk-begin seq))))
                          (<change>
                             (if (null? seq)
                                seq
                                (if (last-exp? seq)
                                   (first-exp seq)
                                   (mk-begin seq)))
                             @sensitivity:FA)))
         (application? (lambda (exp)
                         @sensitivity:FA
                         (pair? exp)))
         (operator (lambda (exp)
                     (<change>
                        @sensitivity:FA
                        ())
                     (car exp)))
         (operands (lambda (exp)
                     (<change>
                        @sensitivity:FA
                        ((lambda (x) x) @sensitivity:FA))
                     (cdr exp)))
         (no-operands? (lambda (ops)
                         @sensitivity:FA
                         (null? ops)))
         (first-operand (lambda (ops)
                          @sensitivity:FA
                          (car ops)))
         (rest-operands (lambda (ops)
                          @sensitivity:FA
                          (cdr ops)))
         (cond? (lambda (exp)
                  @sensitivity:FA
                  (tagged-list? exp 'cond)))
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
                                 ((lambda (x) x) @sensitivity:FA))
                              (eq? (cond-predicate clause) 'else)))
         (cond-actions (lambda (clause)
                         (<change>
                            @sensitivity:FA
                            ())
                         (cdr clause)))
         (expand-clauses (lambda (clauses)
                           @sensitivity:No
                           (if (null? clauses)
                              'false
                              (let ((first (car clauses))
                                    (rest (cdr clauses)))
                                 (<change>
                                    (if (cond-else-clause? first)
                                       (if (null? rest)
                                          (sequence->exp (cond-actions first))
                                          (error "ELSE clause isn't last -- COND->IF" clauses))
                                       (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))
                                    ((lambda (x) x)
                                       (if (cond-else-clause? first)
                                          (if (null? rest)
                                             (sequence->exp (cond-actions first))
                                             (error "ELSE clause isn't last -- COND->IF" clauses))
                                          (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))))))
         (cond->if (lambda (exp)
                     (<change>
                        @sensitivity:FA
                        (expand-clauses (cond-clauses exp)))
                     (<change>
                        (expand-clauses (cond-clauses exp))
                        @sensitivity:FA)))
         (true? (lambda (x)
                  @sensitivity:FA
                  (not (eq? x #f))))
         (false? (lambda (x)
                   @sensitivity:FA
                   (eq? x #f)))
         (make-procedure (lambda (parameters body env)
                           (<change>
                              @sensitivity:FA
                              (cons 'procedure (cons parameters (cons body (cons env ())))))
                           (<change>
                              (cons 'procedure (cons parameters (cons body (cons env ()))))
                              @sensitivity:FA)))
         (compound-procedure? (lambda (p)
                                (<change>
                                   @sensitivity:FA
                                   ())
                                (tagged-list? p 'procedure)))
         (procedure-parameters (lambda (p)
                                 @sensitivity:FA
                                 (cadr p)))
         (procedure-body (lambda (p)
                           @sensitivity:FA
                           (caddr p)))
         (procedure-environment (lambda (p)
                                  @sensitivity:FA
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
                          ())
                       (cons variables values)))
         (frame-variables (lambda (frame)
                            @sensitivity:FA
                            (car frame)))
         (frame-values (lambda (frame)
                         (<change>
                            @sensitivity:FA
                            ())
                         (cdr frame)))
         (add-binding-to-frame! (lambda (var val frame)
                                  @sensitivity:FA
                                  (<change>
                                     (set-car! frame (cons var (car frame)))
                                     ((lambda (x) x) (set-car! frame (cons var (car frame)))))
                                  (<change>
                                     (set-cdr! frame (cons val (cdr frame)))
                                     ((lambda (x) x) (set-cdr! frame (cons val (cdr frame)))))))
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
                                  @sensitivity:FA
                                  (<change>
                                     (letrec ((env-loop (lambda (env)
                                                          @sensitivity:FA
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
                                        (env-loop env))
                                     ((lambda (x) x)
                                        (letrec ((env-loop (lambda (env)
                                                             @sensitivity:FA
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
                                           (env-loop env))))))
         (set-variable-value! (lambda (var val env)
                                (<change>
                                   @sensitivity:FA
                                   (letrec ((env-loop (lambda (env)
                                                        @sensitivity:FA
                                                        (letrec ((scan (lambda (vars vals)
                                                                         (if (null? vars)
                                                                            (if (not (eq? var (car vars)))
                                                                               (set-car! vals val)
                                                                               (scan (cdr vars) (cdr vals)))
                                                                            (env-loop (enclosing-environment env))))))
                                                           (if (eq? env the-empty-environment)
                                                              (error "Unbound variable -- SET!" var)
                                                              (let ((frame (first-frame env)))
                                                                 (scan (frame-variables frame) (frame-values frame))))))))
                                      (env-loop env)))
                                (<change>
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
                                      (env-loop env))
                                   @sensitivity:FA)))
         (define-variable! (lambda (var val env)
                             @sensitivity:FA
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
                                 (<change>
                                    @sensitivity:FA
                                    ())
                                 (tagged-list? proc 'primitive)))
         (primitive-implementation (lambda (proc)
                                     @sensitivity:FA
                                     (cadr proc)))
         (primitive-procedures (cons (cons '= (cons = ())) (cons (cons '* (cons * ())) (cons (cons '- (cons - ())) ()))))
         (primitive-procedure-names (lambda ()
                                      @sensitivity:FA
                                      (map car primitive-procedures)))
         (primitive-procedure-objects (lambda ()
                                        @sensitivity:FA
                                        (map (lambda (proc) (cons 'primitive (cons (cadr proc) ()))) primitive-procedures)))
         (setup-environment (lambda ()
                              (let ((initial-env (extend-environment
                                                   (primitive-procedure-names)
                                                   (primitive-procedure-objects)
                                                   the-empty-environment)))
                                 (<change>
                                    (define-variable! 'true #t initial-env)
                                    ())
                                 (define-variable! 'false #f initial-env)
                                 initial-env)))
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
                                                      ((lambda (x) x) (mceval (first-exp exps) env)))
                                                   (eval-sequence (rest-exps exps) env))))))
                      (let ((mcapply (lambda (procedure arguments)
                                       @sensitivity:FA
                                       (if (primitive-procedure? procedure)
                                          (apply-primitive-procedure procedure arguments)
                                          (if (compound-procedure? procedure)
                                             (eval-sequence
                                                (procedure-body procedure)
                                                (extend-environment (procedure-parameters procedure) arguments (procedure-environment procedure)))
                                             (error "Unknown procedure type -- APPLY" procedure))))))
                         (let ((eval-if (lambda (exp env)
                                          @sensitivity:FA
                                          (if (true? (mceval (if-predicate exp) env))
                                             (mceval (if-consequent exp) env)
                                             (mceval (if-alternative exp) env)))))
                            (let ((eval-assignment (lambda (exp env)
                                                     (set-variable-value! (assignment-variable exp) (mceval (assignment-value exp) env) env)
                                                     'ok)))
                               (<change>
                                  ()
                                  (display (list-of-values (rest-operands exps) env)))
                               (let ((eval-definition (lambda (exp env)
                                                        @sensitivity:FA
                                                        (define-variable! (definition-variable exp) (mceval (definition-value exp) env) env)
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