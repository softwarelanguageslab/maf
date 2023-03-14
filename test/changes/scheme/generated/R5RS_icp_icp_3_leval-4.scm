; Changes:
; * removed: 1
; * added: 15
; * swaps: 1
; * negated predicates: 3
; * swapped branches: 1
; * calls to id fun: 9
(letrec ((true #t)
         (false #f)
         (delay-it (lambda (exp env)
                     (list 'thunk exp env)))
         (thunk? (lambda (obj)
                   (<change>
                      ()
                      tagged-list?)
                   (tagged-list? obj 'thunk)))
         (thunk-exp (lambda (thunk)
                      (cadr thunk)))
         (thunk-env (lambda (thunk)
                      (caddr thunk)))
         (force-it (lambda (obj)
                     (if (thunk? obj)
                        (actual-value (thunk-exp obj) (thunk-env obj))
                        obj)))
         (nonmemo-force-it force-it)
         (evaluated-thunk? (lambda (obj)
                             (<change>
                                ()
                                obj)
                             (<change>
                                ()
                                (display (tagged-list? obj 'evaluated-thunk)))
                             (tagged-list? obj 'evaluated-thunk)))
         (thunk-value (lambda (evaluated-thunk)
                        (cadr evaluated-thunk)))
         (force-it2 (lambda (obj)
                      (if (thunk? obj)
                         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
                            (set-car! obj 'evaluated-thunk)
                            (<change>
                               ()
                               obj)
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
                                   (if (lambda? exp)
                                      (make-procedure (lambda-parameters exp) (lambda-body exp) env)
                                      (if (begin? exp)
                                         (eval-sequence (begin-actions exp) env)
                                         (if (cond? exp)
                                            (eval (cond->if exp) env)
                                            (if (application? exp)
                                               (leval-apply (actual-value (operator exp) env) (operands exp) env)
                                               (error "Unknown expression type -- EVAL")))))))))))))
         (actual-value (lambda (exp env)
                         (force-it (eval exp env))))
         (leval-apply (lambda (procedure arguments env)
                        (<change>
                           (if (primitive-procedure? procedure)
                              (apply-primitive-procedure procedure (list-of-arg-values arguments env))
                              (if (compound-procedure? procedure)
                                 (eval-sequence
                                    (procedure-body procedure)
                                    (extend-environment
                                       (procedure-parameters procedure)
                                       (list-of-delayed-args arguments env)
                                       (procedure-environment procedure)))
                                 (error "Unknown procedure type -- APPLY")))
                           ((lambda (x) x)
                              (if (primitive-procedure? procedure)
                                 (apply-primitive-procedure procedure (list-of-arg-values arguments env))
                                 (if (compound-procedure? procedure)
                                    (eval-sequence
                                       (procedure-body procedure)
                                       (extend-environment
                                          (procedure-parameters procedure)
                                          (list-of-delayed-args arguments env)
                                          (procedure-environment procedure)))
                                    (error "Unknown procedure type -- APPLY")))))))
         (list-of-arg-values (lambda (exps env)
                               (if (<change> (no-operands? exps) (not (no-operands? exps)))
                                  ()
                                  (cons (actual-value (first-operand exps) env) (list-of-arg-values (rest-operands exps) env)))))
         (list-of-delayed-args (lambda (exps env)
                                 (if (no-operands? exps)
                                    ()
                                    (cons (delay-it (first-operand exps) env) (list-of-delayed-args (rest-operands exps) env)))))
         (eval-assignment (lambda (exp env)
                            (<change>
                               (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env)
                               'ok)
                            (<change>
                               'ok
                               (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env))))
         (eval-definition (lambda (exp env)
                            (define-variable! (definition-variable exp) (eval (definition-value exp) env) env)
                            'ok))
         (true? (lambda (x)
                  (not (eq? x false))))
         (false? (lambda (x)
                   (eq? x false)))
         (eval-if (lambda (exp env)
                    (<change>
                       ()
                       actual-value)
                    (if (true? (actual-value (if-predicate exp) env))
                       (eval (if-consequent exp) env)
                       (eval (if-alternative exp) env))))
         (eval-sequence (lambda (exps env)
                          (if (last-exp? exps)
                             (eval (first-exp exps) env)
                             (begin
                                (<change>
                                   ()
                                   (display eval-sequence))
                                (eval (first-exp exps) env)
                                (eval-sequence (rest-exps exps) env)))))
         (self-evaluating? (lambda (exp)
                             (if (number? exp)
                                true
                                (if (string? exp) true false))))
         (tagged-list? (lambda (exp tag)
                         (<change>
                            (if (pair? exp) (eq? (car exp) tag) false)
                            ((lambda (x) x) (if (pair? exp) (eq? (car exp) tag) false)))))
         (quoted? (lambda (exp)
                    (tagged-list? exp 'quote)))
         (text-of-quotation (lambda (exp)
                              (cadr exp)))
         (quasiquoted? (lambda (exp)
                         (tagged-list? exp 'quasiquote)))
         (unquote? (lambda (exp)
                     (tagged-list? exp 'unquote)))
         (variable? (lambda (exp)
                      (symbol? exp)))
         (assignment? (lambda (exp)
                        (tagged-list? exp 'set!)))
         (assignment-variable (lambda (exp)
                                (<change>
                                   ()
                                   cadr)
                                (cadr exp)))
         (assignment-value (lambda (exp)
                             (<change>
                                (caddr exp)
                                ((lambda (x) x) (caddr exp)))))
         (definition? (lambda (exp)
                        (tagged-list? exp 'define)))
         (definition-variable (lambda (exp)
                                (<change>
                                   ()
                                   (display cadr))
                                (if (<change> (symbol? (cadr exp)) (not (symbol? (cadr exp))))
                                   (cadr exp)
                                   (caadr exp))))
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
                              (<change>
                                 (cadddr exp)
                                 'false)
                              (<change>
                                 'false
                                 (cadddr exp)))))
         (make-if (lambda (predicate consequent alternative)
                    (list 'if predicate consequent alternative)))
         (lambda? (lambda (exp)
                    (<change>
                       (tagged-list? exp 'lambda)
                       ((lambda (x) x) (tagged-list? exp 'lambda)))))
         (lambda-parameters (lambda (exp)
                              (cadr exp)))
         (lambda-body (lambda (exp)
                        (<change>
                           (cddr exp)
                           ((lambda (x) x) (cddr exp)))))
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
                           (<change>
                              ()
                              (error "ELSE clause isn't last -- COND->IF"))
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
                      (cdr seq)))
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
                            (null? ops)
                            ((lambda (x) x) (null? ops)))))
         (first-operand (lambda (ops)
                          (car ops)))
         (rest-operands (lambda (ops)
                          (cdr ops)))
         (make-procedure (lambda (parameters body env)
                           (list 'procedure parameters body env)))
         (compound-procedure? (lambda (p)
                                (<change>
                                   ()
                                   (display 'procedure))
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
                        (<change>
                           (car env)
                           ((lambda (x) x) (car env)))))
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
                                     set-cdr!)
                                  (set-cdr! frame (cons val (cdr frame)))))
         (lookup-variable-value (lambda (var env)
                                  (letrec ((env-loop (lambda (env)
                                                       (letrec ((scan (lambda (vars vals)
                                                                        (if (<change> (null? vars) (not (null? vars)))
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
                              (<change>
                                 (let ((initial-env (extend-environment
                                                      (primitive-procedure-names)
                                                      (primitive-procedure-objects)
                                                      the-empty-environment)))
                                    (define-variable! 'true true initial-env)
                                    (define-variable! 'false false initial-env)
                                    initial-env)
                                 ((lambda (x) x)
                                    (let ((initial-env (extend-environment
                                                         (primitive-procedure-names)
                                                         (primitive-procedure-objects)
                                                         the-empty-environment)))
                                       (define-variable! 'true true initial-env)
                                       (<change>
                                          ()
                                          (display 'true))
                                       (define-variable! 'false false initial-env)
                                       (<change>
                                          initial-env
                                          ((lambda (x) x) initial-env)))))))
         (primitive-procedure? (lambda (proc)
                                 (tagged-list? proc 'primitive)))
         (primitive-implementation (lambda (proc)
                                     (<change>
                                        ()
                                        proc)
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
                                        (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)))
         (apply-primitive-procedure (lambda (proc args)
                                      (apply-in-underlying-scheme (primitive-implementation proc) args)))
         (input-prompt ";;; L-Eval input:")
         (output-prompt ";;; L-Eval value:")
         (prompt-for-input (lambda (string)
                             (newline)
                             (newline)
                             (display string)
                             (newline)))
         (announce-output (lambda (string)
                            (<change>
                               (newline)
                               ())
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
                                        (<change>
                                           ()
                                           qargs)
                                        qargs))
         (aware-eval-quotation-args (lambda (qargs env)
                                      (if (pair? qargs)
                                         (eval (make-list qargs) env)
                                         qargs)))
         (eval-quotation-args unaware-eval-quotation-args))
   (<change>
      ()
      (__toplevel_cons 'm ()))
   (display "Loading definitions of cons, car, cdr as compound procedures. ")
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