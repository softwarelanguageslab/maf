; Changes:
; * removed: 1
; * added: 9
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 5
; * calls to id fun: 14
(letrec ((eval (lambda (exp env)
                 (<change>
                    ((analyze exp) env)
                    ((lambda (x) x) ((analyze exp) env)))))
         (analyze (lambda (exp)
                    (<change>
                       ()
                       (application? exp))
                    (if (self-evaluating? exp)
                       (analyze-self-evaluating exp)
                       (if (quoted? exp)
                          (analyze-quoted exp)
                          (if (variable? exp)
                             (analyze-variable exp)
                             (if (assignment? exp)
                                (analyze-assignment exp)
                                (if (definition? exp)
                                   (analyze-definition exp)
                                   (if (if? exp)
                                      (analyze-if exp)
                                      (if (lambda? exp)
                                         (analyze-lambda exp)
                                         (if (begin? exp)
                                            (analyze-sequence (begin-actions exp))
                                            (if (cond? exp)
                                               (analyze (cond->if exp))
                                               (if (application? exp)
                                                  (analyze-application exp)
                                                  (error "Unknown expression type -- ANALYZE")))))))))))))
         (analyze-self-evaluating (lambda (exp)
                                    (lambda (env)
                                       exp)))
         (analyze-quoted (lambda (exp)
                           (<change>
                              (let ((qval (text-of-quotation exp)))
                                 (lambda (env)
                                    qval))
                              ((lambda (x) x) (let ((qval (text-of-quotation exp))) (<change> () qval) (lambda (env) qval))))))
         (analyze-variable (lambda (exp)
                             (lambda (env)
                                (lookup-variable-value exp env))))
         (analyze-assignment (lambda (exp)
                               (let ((var (assignment-variable exp))
                                     (vproc (analyze (assignment-value exp))))
                                  (lambda (env)
                                     (set-variable-value! var (vproc env) env)
                                     'ok))))
         (analyze-definition (lambda (exp)
                               (let ((var (definition-variable exp))
                                     (vproc (analyze (definition-value exp))))
                                  (<change>
                                     ()
                                     (display (lambda (env) (define-variable! var (vproc env) env) 'ok)))
                                  (<change>
                                     (lambda (env)
                                        (define-variable! var (vproc env) env)
                                        'ok)
                                     ((lambda (x) x) (lambda (env) (define-variable! var (vproc env) env) 'ok))))))
         (analyze-if (lambda (exp)
                       (let ((pproc (analyze (if-predicate exp)))
                             (cproc (analyze (if-consequent exp)))
                             (aproc (analyze (if-alternative exp))))
                          (lambda (env)
                             (if (true? (pproc env)) (cproc env) (aproc env))))))
         (analyze-lambda (lambda (exp)
                           (let ((vars (lambda-parameters exp))
                                 (bproc (analyze-sequence (lambda-body exp))))
                              (lambda (env)
                                 (make-procedure vars bproc env)))))
         (analyze-sequence (lambda (exps)
                             (letrec ((sequentially (lambda (proc1 proc2)
                                                      (lambda (env)
                                                         (<change>
                                                            (proc1 env)
                                                            ((lambda (x) x) (proc1 env)))
                                                         (proc2 env))))
                                      (loop (lambda (first-proc rest-procs)
                                              (if (null? rest-procs)
                                                 first-proc
                                                 (loop (sequentially first-proc (car rest-procs)) (cdr rest-procs))))))
                                (let ((procs (map analyze exps)))
                                   (if (null? procs)
                                      (error "Empty sequence -- ANALYZE")
                                      #f)
                                   (<change>
                                      (loop (car procs) (cdr procs))
                                      ((lambda (x) x) (loop (car procs) (cdr procs))))))))
         (analyze-application (lambda (exp)
                                (<change>
                                   ()
                                   (display env))
                                (let ((fproc (analyze (operator exp)))
                                      (aprocs (map analyze (operands exp))))
                                   (lambda (env)
                                      (execute-application (fproc env) (map (lambda (aproc) (aproc env)) aprocs))))))
         (execute-application (lambda (proc args)
                                (<change>
                                   (if (primitive-procedure? proc)
                                      (apply-primitive-procedure proc args)
                                      (if (compound-procedure? proc)
                                         ((procedure-body proc)
                                            (extend-environment (procedure-parameters proc) args (procedure-environment proc)))
                                         (error "Unknown procedure type -- EXECUTE-APPLICATION")))
                                   ((lambda (x) x)
                                      (if (primitive-procedure? proc)
                                         (apply-primitive-procedure proc args)
                                         (if (<change> (compound-procedure? proc) (not (compound-procedure? proc)))
                                            ((procedure-body proc)
                                               (extend-environment (procedure-parameters proc) args (procedure-environment proc)))
                                            (error "Unknown procedure type -- EXECUTE-APPLICATION")))))))
         (true #t)
         (false #f)
         (list-of-values (lambda (exps env)
                           (if (<change> (no-operands? exps) (not (no-operands? exps)))
                              ()
                              (cons (eval (first-operand exps) env) (list-of-values (rest-operands exps) env)))))
         (eval-assignment (lambda (exp env)
                            (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env)
                            (<change>
                               ()
                               env)
                            'ok))
         (eval-definition (lambda (exp env)
                            (define-variable! (definition-variable exp) (eval (definition-value exp) env) env)
                            'ok))
         (true? (lambda (x)
                  (not (eq? x false))))
         (false? (lambda (x)
                   (eq? x false)))
         (eval-if (lambda (exp env)
                    (if (true? (eval (if-predicate exp) env))
                       (<change>
                          (eval (if-consequent exp) env)
                          (eval (if-alternative exp) env))
                       (<change>
                          (eval (if-alternative exp) env)
                          (eval (if-consequent exp) env)))))
         (eval-sequence (lambda (exps env)
                          (if (last-exp? exps)
                             (eval (first-exp exps) env)
                             (begin
                                (eval (first-exp exps) env)
                                (eval-sequence (rest-exps exps) env)))))
         (self-evaluating? (lambda (exp)
                             (<change>
                                ()
                                exp)
                             (if (number? exp)
                                true
                                (if (string? exp)
                                   (<change>
                                      true
                                      false)
                                   (<change>
                                      false
                                      true)))))
         (tagged-list? (lambda (exp tag)
                         (if (pair? exp) (eq? (car exp) tag) false)))
         (quoted? (lambda (exp)
                    (tagged-list? exp 'quote)))
         (text-of-quotation (lambda (exp)
                              (cadr exp)))
         (variable? (lambda (exp)
                      (<change>
                         (symbol? exp)
                         ((lambda (x) x) (symbol? exp)))))
         (assignment? (lambda (exp)
                        (tagged-list? exp 'set!)))
         (assignment-variable (lambda (exp)
                                (cadr exp)))
         (assignment-value (lambda (exp)
                             (caddr exp)))
         (definition? (lambda (exp)
                        (<change>
                           (tagged-list? exp 'define)
                           ((lambda (x) x) (tagged-list? exp 'define)))))
         (definition-variable (lambda (exp)
                                (if (symbol? (cadr exp)) (cadr exp) (caadr exp))))
         (definition-value (lambda (exp)
                             (<change>
                                (if (symbol? (cadr exp))
                                   (caddr exp)
                                   (make-lambda (cdadr exp) (cddr exp)))
                                ((lambda (x) x) (if (symbol? (cadr exp)) (caddr exp) (make-lambda (cdadr exp) (cddr exp)))))))
         (if? (lambda (exp)
                (tagged-list? exp 'if)))
         (if-predicate (lambda (exp)
                         (cadr exp)))
         (if-consequent (lambda (exp)
                          (caddr exp)))
         (if-alternative (lambda (exp)
                           (if (not (null? (cdddr exp)))
                              (cadddr exp)
                              'false)))
         (make-if (lambda (predicate consequent alternative)
                    (list 'if predicate consequent alternative)))
         (lambda? (lambda (exp)
                    (tagged-list? exp 'lambda)))
         (lambda-parameters (lambda (exp)
                              (cadr exp)))
         (lambda-body (lambda (exp)
                        (cddr exp)))
         (make-lambda (lambda (parameters body)
                        (cons 'lambda (cons parameters body))))
         (cond? (lambda (exp)
                  (tagged-list? exp 'cond)))
         (cond-clauses (lambda (exp)
                         (cdr exp)))
         (cond-else-clause? (lambda (clause)
                              (eq? (cond-predicate clause) 'else)))
         (cond-predicate (lambda (clause)
                           (car clause)))
         (cond-actions (lambda (clause)
                         (cdr clause)))
         (cond->if (lambda (exp)
                     (expand-clauses (cond-clauses exp))))
         (expand-clauses (lambda (clauses)
                           (if (null? clauses)
                              (<change>
                                 'false
                                 (let ((first (car clauses))
                                       (rest (cdr clauses)))
                                    (if (cond-else-clause? first)
                                       (if (null? rest)
                                          (sequence->exp (cond-actions first))
                                          (error "ELSE clause isn't last -- COND->IF"))
                                       (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))
                              (<change>
                                 (let ((first (car clauses))
                                       (rest (cdr clauses)))
                                    (if (cond-else-clause? first)
                                       (if (null? rest)
                                          (sequence->exp (cond-actions first))
                                          (error "ELSE clause isn't last -- COND->IF"))
                                       (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest))))
                                 'false))))
         (begin? (lambda (exp)
                   (<change>
                      (tagged-list? exp 'begin)
                      ((lambda (x) x) (tagged-list? exp 'begin)))))
         (begin-actions (lambda (exp)
                          (<change>
                             ()
                             cdr)
                          (cdr exp)))
         (last-exp? (lambda (seq)
                      (null? (cdr seq))))
         (first-exp (lambda (seq)
                      (car seq)))
         (rest-exps (lambda (seq)
                      (cdr seq)))
         (sequence->exp (lambda (seq)
                          (if (null? seq)
                             seq
                             (if (last-exp? seq)
                                (<change>
                                   (first-exp seq)
                                   (make-begin seq))
                                (<change>
                                   (make-begin seq)
                                   (first-exp seq))))))
         (make-begin (lambda (seq)
                       (cons 'begin seq)))
         (application? (lambda (exp)
                         (pair? exp)))
         (operator (lambda (exp)
                     (car exp)))
         (operands (lambda (exp)
                     (cdr exp)))
         (no-operands? (lambda (ops)
                         (null? ops)))
         (first-operand (lambda (ops)
                          (<change>
                             (car ops)
                             ((lambda (x) x) (car ops)))))
         (rest-operands (lambda (ops)
                          (<change>
                             (cdr ops)
                             ((lambda (x) x) (cdr ops)))))
         (make-procedure (lambda (parameters body env)
                           (list 'procedure parameters body env)))
         (compound-procedure? (lambda (p)
                                (tagged-list? p 'procedure)))
         (procedure-parameters (lambda (p)
                                 (cadr p)))
         (procedure-body (lambda (p)
                           (caddr p)))
         (procedure-environment (lambda (p)
                                  (cadddr p)))
         (enclosing-environment (lambda (env)
                                  (cdr env)))
         (first-frame (lambda (env)
                        (car env)))
         (the-empty-environment ())
         (extend-environment (lambda (vars vals base-env)
                               (if (= (length vars) (length vals))
                                  (cons (make-frame vars vals) base-env)
                                  (if (< (length vars) (length vals))
                                     (error "Too many arguments supplied")
                                     (error "Too few arguments supplied")))))
         (make-frame (lambda (variables values)
                       (cons variables values)))
         (frame-variables (lambda (frame)
                            (car frame)))
         (frame-values (lambda (frame)
                         (cdr frame)))
         (add-binding-to-frame! (lambda (var val frame)
                                  (set-car! frame (cons var (car frame)))
                                  (<change>
                                     ()
                                     (display (set-car! frame (cons var (car frame)))))
                                  (set-cdr! frame (cons val (cdr frame)))))
         (lookup-variable-value (lambda (var env)
                                  (letrec ((env-loop (lambda (env)
                                                       (letrec ((scan (lambda (vars vals)
                                                                        (if (null? vars)
                                                                           (env-loop (enclosing-environment env))
                                                                           (if (eq? var (car vars))
                                                                              (car vals)
                                                                              (scan (cdr vars) (cdr vals)))))))
                                                          (if (eq? env the-empty-environment)
                                                             (error "Unbound variable")
                                                             (let ((frame (first-frame env)))
                                                                (scan (frame-variables frame) (frame-values frame))))))))
                                     (env-loop env))))
         (set-variable-value! (lambda (var val env)
                                (letrec ((env-loop (lambda (env)
                                                     (letrec ((scan (lambda (vars vals)
                                                                      (if (null? vars)
                                                                         (env-loop (enclosing-environment env))
                                                                         (if (eq? var (car vars))
                                                                            (set-car! vals val)
                                                                            (scan (cdr vars) (cdr vals)))))))
                                                        (if (eq? env the-empty-environment)
                                                           (error "Unbound variable -- SET!")
                                                           (let ((frame (first-frame env)))
                                                              (scan (frame-variables frame) (frame-values frame))))))))
                                   (env-loop env))))
         (define-variable! (lambda (var val env)
                             (let ((frame (first-frame env)))
                                (letrec ((scan (lambda (vars vals)
                                                 (if (null? vars)
                                                    (add-binding-to-frame! var val frame)
                                                    (if (eq? var (car vars))
                                                       (set-car! vals val)
                                                       (scan (cdr vars) (cdr vals)))))))
                                   (scan (frame-variables frame) (frame-values frame))))))
         (setup-environment (lambda ()
                              (let ((initial-env (extend-environment
                                                   (primitive-procedure-names)
                                                   (primitive-procedure-objects)
                                                   the-empty-environment)))
                                 (<change>
                                    (define-variable! 'true true initial-env)
                                    ())
                                 (define-variable! 'false false initial-env)
                                 initial-env)))
         (primitive-procedure? (lambda (proc)
                                 (tagged-list? proc 'primitive)))
         (primitive-implementation (lambda (proc)
                                     (cadr proc)))
         (primitive-procedures (list
                                 (list 'car car)
                                 (list 'cdr cdr)
                                 (list 'cons cons)
                                 (list 'null? null?)
                                 (list '+ +)
                                 (list '- -)
                                 (list '= =)
                                 (list '* *)))
         (primitive-procedure-names (lambda ()
                                      (map car primitive-procedures)))
         (primitive-procedure-objects (lambda ()
                                        (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)))
         (apply-primitive-procedure (lambda (proc args)
                                      (if (null? args)
                                         ((primitive-implementation proc))
                                         (if (null? (cdr args))
                                            (<change>
                                               ((primitive-implementation proc) (car args))
                                               (if (null? (cddr args))
                                                  ((primitive-implementation proc) (car args) (cadr args))
                                                  (error "Unsupported call")))
                                            (<change>
                                               (if (null? (cddr args))
                                                  ((primitive-implementation proc) (car args) (cadr args))
                                                  (error "Unsupported call"))
                                               ((primitive-implementation proc) (car args)))))))
         (input-prompt ";;; Analyzing-Eval input:")
         (output-prompt ";;; Analyzing-Eval value:")
         (prompt-for-input (lambda (string)
                             (newline)
                             (<change>
                                ()
                                display)
                             (newline)
                             (display string)
                             (newline)))
         (announce-output (lambda (string)
                            (newline)
                            (<change>
                               (display string)
                               ((lambda (x) x) (display string)))
                            (newline)))
         (user-print (lambda (object)
                       (if (compound-procedure? object)
                          (display (list 'compound-procedure (procedure-parameters object) (procedure-body object) '<env>))
                          (display object))))
         (the-global-environment (setup-environment)))
   (<change>
      (eval
         (__toplevel_cons
            'begin
            (__toplevel_cons
               (__toplevel_cons
                  'define
                  (__toplevel_cons
                     (__toplevel_cons 'fac (__toplevel_cons 'num ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           'if
                           (__toplevel_cons
                              (__toplevel_cons '= (__toplevel_cons 'num (__toplevel_cons 0 ())))
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       '*
                                       (__toplevel_cons
                                          'num
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'fac
                                                (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'num (__toplevel_cons 1 ()))) ()))
                                             ())))
                                    ()))))
                        ())))
               (__toplevel_cons
                  (__toplevel_cons
                     'define
                     (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'fac (__toplevel_cons 10000 ())) ())))
                  ())))
         the-global-environment)
      ((lambda (x) x)
         (eval
            (__toplevel_cons
               'begin
               (__toplevel_cons
                  (__toplevel_cons
                     'define
                     (__toplevel_cons
                        (__toplevel_cons 'fac (__toplevel_cons 'num ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              'if
                              (__toplevel_cons
                                 (__toplevel_cons '= (__toplevel_cons 'num (__toplevel_cons 0 ())))
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          '*
                                          (__toplevel_cons
                                             'num
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   'fac
                                                   (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'num (__toplevel_cons 1 ()))) ()))
                                                ())))
                                       ()))))
                           ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'define
                        (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'fac (__toplevel_cons 10000 ())) ())))
                     ())))
            the-global-environment))))