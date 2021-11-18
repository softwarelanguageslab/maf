; Changes:
; * removed: 2
; * added: 3
; * swaps: 2
; * negated predicates: 2
; * swapped branches: 1
; * calls to id fun: 11
(letrec ((true #t)
         (false #f)
         (delay-it (lambda (exp env)
                     (list 'thunk exp env)))
         (thunk? (lambda (obj)
                   (tagged-list? obj 'thunk)))
         (thunk-exp (lambda (thunk)
                      (cadr thunk)))
         (thunk-env (lambda (thunk)
                      (<change>
                         ()
                         caddr)
                      (caddr thunk)))
         (force-it (lambda (obj)
                     (if (thunk? obj)
                        (actual-value (thunk-exp obj) (thunk-env obj))
                        obj)))
         (nonmemo-force-it force-it)
         (evaluated-thunk? (lambda (obj)
                             (tagged-list? obj 'evaluated-thunk)))
         (thunk-value (lambda (evaluated-thunk)
                        (cadr evaluated-thunk)))
         (force-it2 (lambda (obj)
                      (if (thunk? obj)
                         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
                            (set-car! obj 'evaluated-thunk)
                            (set-car! (cdr obj) result)
                            (set-cdr! (cdr obj) ())
                            result)
                         (if (evaluated-thunk? obj) (thunk-value obj) obj))))
         (memo-force-it force-it2)
         (apply-in-underlying-scheme (lambda (proc args)
                                       (if (null? args)
                                          (proc)
                                          (if (null? (cdr args))
                                             (proc (car args))
                                             (if (null? (cddr args))
                                                (proc (car args) (cadr args))
                                                (error "Unsupported call"))))))
         (eval (lambda (exp env)
                 (if (self-evaluating? exp)
                    exp
                    (if (variable? exp)
                       (lookup-variable-value exp env)
                       (if (let ((__or_res (quoted? exp))) (if __or_res __or_res (quasiquoted? exp)))
                          (eval-quotation-args (text-of-quotation exp) env)
                          (if (assignment? exp)
                             (eval-assignment exp env)
                             (if (definition? exp)
                                (eval-definition exp env)
                                (if (if? exp)
                                   (eval-if exp env)
                                   (if (<change> (lambda? exp) (not (lambda? exp)))
                                      (make-procedure (lambda-parameters exp) (lambda-body exp) env)
                                      (if (begin? exp)
                                         (eval-sequence (begin-actions exp) env)
                                         (if (<change> (cond? exp) (not (cond? exp)))
                                            (eval (cond->if exp) env)
                                            (if (application? exp)
                                               (leval-apply (actual-value (operator exp) env) (operands exp) env)
                                               (error "Unknown expression type -- EVAL")))))))))))))
         (actual-value (lambda (exp env)
                         (force-it (eval exp env))))
         (leval-apply (lambda (procedure arguments env)
                        (if (primitive-procedure? procedure)
                           (apply-primitive-procedure procedure (list-of-arg-values arguments env))
                           (if (compound-procedure? procedure)
                              (eval-sequence
                                 (procedure-body procedure)
                                 (extend-environment
                                    (procedure-parameters procedure)
                                    (list-of-delayed-args arguments env)
                                    (procedure-environment procedure)))
                              (error "Unknown procedure type -- APPLY")))))
         (list-of-arg-values (lambda (exps env)
                               (if (no-operands? exps)
                                  ()
                                  (cons (actual-value (first-operand exps) env) (list-of-arg-values (rest-operands exps) env)))))
         (list-of-delayed-args (lambda (exps env)
                                 (if (no-operands? exps)
                                    ()
                                    (cons (delay-it (first-operand exps) env) (list-of-delayed-args (rest-operands exps) env)))))
         (eval-assignment (lambda (exp env)
                            (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env)
                            'ok))
         (eval-definition (lambda (exp env)
                            (define-variable! (definition-variable exp) (eval (definition-value exp) env) env)
                            'ok))
         (true? (lambda (x)
                  (not (eq? x false))))
         (false? (lambda (x)
                   (eq? x false)))
         (eval-if (lambda (exp env)
                    (if (true? (actual-value (if-predicate exp) env))
                       (eval (if-consequent exp) env)
                       (eval (if-alternative exp) env))))
         (eval-sequence (lambda (exps env)
                          (<change>
                             (if (last-exp? exps)
                                (eval (first-exp exps) env)
                                (begin
                                   (eval (first-exp exps) env)
                                   (eval-sequence (rest-exps exps) env)))
                             ((lambda (x) x)
                                (if (last-exp? exps)
                                   (eval (first-exp exps) env)
                                   (begin
                                      (eval (first-exp exps) env)
                                      (eval-sequence (rest-exps exps) env)))))))
         (self-evaluating? (lambda (exp)
                             (if (number? exp)
                                (<change>
                                   true
                                   (if (string? exp) true false))
                                (<change>
                                   (if (string? exp) true false)
                                   true))))
         (tagged-list? (lambda (exp tag)
                         (if (pair? exp) (eq? (car exp) tag) false)))
         (quoted? (lambda (exp)
                    (<change>
                       (tagged-list? exp 'quote)
                       ((lambda (x) x) (tagged-list? exp 'quote)))))
         (text-of-quotation (lambda (exp)
                              (<change>
                                 (cadr exp)
                                 ((lambda (x) x) (cadr exp)))))
         (quasiquoted? (lambda (exp)
                         (<change>
                            (tagged-list? exp 'quasiquote)
                            ((lambda (x) x) (tagged-list? exp 'quasiquote)))))
         (unquote? (lambda (exp)
                     (tagged-list? exp 'unquote)))
         (variable? (lambda (exp)
                      (symbol? exp)))
         (assignment? (lambda (exp)
                        (tagged-list? exp 'set!)))
         (assignment-variable (lambda (exp)
                                (cadr exp)))
         (assignment-value (lambda (exp)
                             (caddr exp)))
         (definition? (lambda (exp)
                        (tagged-list? exp 'define)))
         (definition-variable (lambda (exp)
                                (if (symbol? (cadr exp)) (cadr exp) (caadr exp))))
         (definition-value (lambda (exp)
                             (if (symbol? (cadr exp))
                                (caddr exp)
                                (make-lambda (cdadr exp) (cddr exp)))))
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
                  (<change>
                     (tagged-list? exp 'cond)
                     ((lambda (x) x) (tagged-list? exp 'cond)))))
         (cond-clauses (lambda (exp)
                         (cdr exp)))
         (cond-else-clause? (lambda (clause)
                              (eq? (cond-predicate clause) 'else)))
         (cond-predicate (lambda (clause)
                           (car clause)))
         (cond-actions (lambda (clause)
                         (cdr clause)))
         (cond->if (lambda (exp)
                     (<change>
                        (expand-clauses (cond-clauses exp))
                        ((lambda (x) x) (expand-clauses (cond-clauses exp))))))
         (expand-clauses (lambda (clauses)
                           (if (null? clauses)
                              'false
                              (let ((first (car clauses))
                                    (rest (cdr clauses)))
                                 (if (cond-else-clause? first)
                                    (if (null? rest)
                                       (sequence->exp (cond-actions first))
                                       (error "ELSE clause isn't last -- COND->IF"))
                                    (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))))
         (begin? (lambda (exp)
                   (tagged-list? exp 'begin)))
         (begin-actions (lambda (exp)
                          (cdr exp)))
         (last-exp? (lambda (seq)
                      (null? (cdr seq))))
         (first-exp (lambda (seq)
                      (car seq)))
         (rest-exps (lambda (seq)
                      (<change>
                         (cdr seq)
                         ((lambda (x) x) (cdr seq)))))
         (sequence->exp (lambda (seq)
                          (if (null? seq)
                             seq
                             (if (last-exp? seq)
                                (first-exp seq)
                                (make-begin seq)))))
         (make-begin (lambda (seq)
                       (cons 'begin seq)))
         (application? (lambda (exp)
                         (pair? exp)))
         (operator (lambda (exp)
                     (car exp)))
         (operands (lambda (exp)
                     (cdr exp)))
         (no-operands? (lambda (ops)
                         (<change>
                            ()
                            (display null?))
                         (<change>
                            (null? ops)
                            ((lambda (x) x) (null? ops)))))
         (first-operand (lambda (ops)
                          (car ops)))
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
                                  (<change>
                                     (set-car! frame (cons var (car frame)))
                                     (set-cdr! frame (cons val (cdr frame))))
                                  (<change>
                                     (set-cdr! frame (cons val (cdr frame)))
                                     (set-car! frame (cons var (car frame))))))
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
                                     (<change>
                                        (env-loop env)
                                        ((lambda (x) x) (env-loop env))))))
         (set-variable-value! (lambda (var val env)
                                (letrec ((env-loop (lambda (env)
                                                     (letrec ((scan (lambda (vars vals)
                                                                      (<change>
                                                                         ()
                                                                         (display car))
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
                                 (define-variable! 'true true initial-env)
                                 (<change>
                                    (define-variable! 'false false initial-env)
                                    ())
                                 initial-env)))
         (primitive-procedure? (lambda (proc)
                                 (tagged-list? proc 'primitive)))
         (primitive-implementation (lambda (proc)
                                     (cadr proc)))
         (primitive-procedures (list
                                 (list 'null? null?)
                                 (list '+ +)
                                 (list '- -)
                                 (list '* *)
                                 (list '/ /)
                                 (list '= =)
                                 (list '<= <=)
                                 (list '>= >=)
                                 (list '< <)
                                 (list '> >)
                                 (list 'newline newline)
                                 (list 'display display)))
         (primitive-procedure-names (lambda ()
                                      (map car primitive-procedures)))
         (primitive-procedure-objects (lambda ()
                                        (<change>
                                           (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)
                                           ((lambda (x) x) (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)))))
         (apply-primitive-procedure (lambda (proc args)
                                      (apply-in-underlying-scheme (primitive-implementation proc) args)))
         (input-prompt ";;; L-Eval input:")
         (output-prompt ";;; L-Eval value:")
         (prompt-for-input (lambda (string)
                             (<change>
                                (newline)
                                (newline))
                             (<change>
                                (newline)
                                (newline))
                             (display string)
                             (newline)))
         (announce-output (lambda (string)
                            (newline)
                            (display string)
                            (newline)))
         (user-print (lambda (object)
                       (if (compound-procedure? object)
                          (display
                             (list 'compound-procedure (procedure-parameters object) (procedure-body object) '<procedure-env>))
                          (display object))))
         (the-global-environment (setup-environment))
         (make-list (lambda (xs)
                      (if (null? xs)
                         ()
                         (let ((x (car xs)))
                            (list 'cons (if (unquote? x) (cadr x) (list 'quote x)) (make-list (cdr xs)))))))
         (unaware-eval-quotation-args (lambda (qargs env)
                                        qargs))
         (aware-eval-quotation-args (lambda (qargs env)
                                      (if (pair? qargs)
                                         (eval (make-list qargs) env)
                                         qargs)))
         (eval-quotation-args unaware-eval-quotation-args))
   (<change>
      (display "Loading definitions of cons, car, cdr as compound procedures. ")
      ())
   (eval
      (__toplevel_cons
         'begin
         (__toplevel_cons
            (__toplevel_cons
               'define
               (__toplevel_cons
                  (__toplevel_cons 'cons (__toplevel_cons 'x (__toplevel_cons 'y ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'lambda
                        (__toplevel_cons
                           (__toplevel_cons 'm ())
                           (__toplevel_cons (__toplevel_cons 'm (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                     ())))
            (__toplevel_cons
               (__toplevel_cons
                  'define
                  (__toplevel_cons
                     (__toplevel_cons 'car (__toplevel_cons 'z ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           'z
                           (__toplevel_cons
                              (__toplevel_cons
                                 'lambda
                                 (__toplevel_cons (__toplevel_cons 'p (__toplevel_cons 'q ())) (__toplevel_cons 'p ())))
                              ()))
                        ())))
               (__toplevel_cons
                  (__toplevel_cons
                     'define
                     (__toplevel_cons
                        (__toplevel_cons 'cdr (__toplevel_cons 'z ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              'z
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'lambda
                                    (__toplevel_cons (__toplevel_cons 'p (__toplevel_cons 'q ())) (__toplevel_cons 'q ())))
                                 ()))
                           ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'define
                        (__toplevel_cons
                           (__toplevel_cons 'list-ref (__toplevel_cons 'items (__toplevel_cons 'n ())))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'if
                                 (__toplevel_cons
                                    (__toplevel_cons '= (__toplevel_cons 'n (__toplevel_cons 0 ())))
                                    (__toplevel_cons
                                       (__toplevel_cons 'car (__toplevel_cons 'items ()))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'list-ref
                                             (__toplevel_cons
                                                (__toplevel_cons 'cdr (__toplevel_cons 'items ()))
                                                (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'n (__toplevel_cons 1 ()))) ())))
                                          ()))))
                              ())))
                     (__toplevel_cons
                        (__toplevel_cons
                           'define
                           (__toplevel_cons
                              (__toplevel_cons 'map (__toplevel_cons 'proc (__toplevel_cons 'items ())))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'if
                                    (__toplevel_cons
                                       (__toplevel_cons 'null? (__toplevel_cons 'items ()))
                                       (__toplevel_cons
                                          (__toplevel_cons 'quote (__toplevel_cons () ()))
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'cons
                                                (__toplevel_cons
                                                   (__toplevel_cons 'proc (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'items ())) ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'map
                                                         (__toplevel_cons 'proc (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'items ())) ())))
                                                      ())))
                                             ()))))
                                 ())))
                        ()))))))
      the-global-environment))