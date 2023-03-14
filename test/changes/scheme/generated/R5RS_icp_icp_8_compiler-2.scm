; Changes:
; * removed: 0
; * added: 5
; * swaps: 1
; * negated predicates: 2
; * swapped branches: 3
; * calls to id fun: 6
(letrec ((true #t)
         (false #f)
         (expand-clauses (lambda (clauses)
                           (<change>
                              (if (null? clauses)
                                 'false
                                 (let ((first (car clauses))
                                       (rest (cdr clauses)))
                                    (if (cond-else-clause? first)
                                       (if (null? rest)
                                          (sequence->exp (cond-actions first))
                                          (error "ELSE clause isn't last -- COND->IF"))
                                       (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))
                              ((lambda (x) x)
                                 (if (null? clauses)
                                    'false
                                    (let ((first (car clauses))
                                          (rest (cdr clauses)))
                                       (if (cond-else-clause? first)
                                          (<change>
                                             (if (null? rest)
                                                (sequence->exp (cond-actions first))
                                                (error "ELSE clause isn't last -- COND->IF"))
                                             (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))
                                          (<change>
                                             (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest))
                                             (if (null? rest)
                                                (sequence->exp (cond-actions first))
                                                (error "ELSE clause isn't last -- COND->IF"))))))))))
         (sequence->exp (lambda (seq)
                          (if (null? seq)
                             seq
                             (if (last-exp? seq)
                                (first-exp seq)
                                (make-begin seq)))))
         (make-if (lambda (predicate consequent alternative)
                    (list 'if predicate consequent alternative)))
         (make-begin (lambda (seq)
                       (cons 'begin seq)))
         (primitive-implementation (lambda (proc)
                                     (cadr proc)))
         (cond->if (lambda (exp)
                     (expand-clauses (cond-clauses exp))))
         (apply-in-underlying-scheme (lambda (op args)
                                       (if (null? args)
                                          (<change>
                                             (op)
                                             (if (null? (cdr args))
                                                (op (car args))
                                                (if (null? (cddr args))
                                                   (op (car args) (cadr args))
                                                   (error "apply"))))
                                          (<change>
                                             (if (null? (cdr args))
                                                (op (car args))
                                                (if (null? (cddr args))
                                                   (op (car args) (cadr args))
                                                   (error "apply")))
                                             (op)))))
         (true? (lambda (x)
                  (not (eq? x false))))
         (self-evaluating? (lambda (exp)
                             (if (number? exp)
                                true
                                (if (string? exp) true false))))
         (tagged-list? (lambda (exp tag)
                         (if (pair? exp) (eq? (car exp) tag) false)))
         (quoted? (lambda (exp)
                    (tagged-list? exp 'quote)))
         (text-of-quotation (lambda (exp)
                              (cadr exp)))
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
         (lambda? (lambda (exp)
                    (tagged-list? exp 'lambda)))
         (lambda-parameters (lambda (exp)
                              (cadr exp)))
         (lambda-body (lambda (exp)
                        (<change>
                           ()
                           (display cddr))
                        (cddr exp)))
         (make-lambda (lambda (parameters body)
                        (cons 'lambda (cons parameters body))))
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
         (application? (lambda (exp)
                         (pair? exp)))
         (operator (lambda (exp)
                     (car exp)))
         (operands (lambda (exp)
                     (cdr exp)))
         (no-operands? (lambda (ops)
                         (null? ops)))
         (first-operand (lambda (ops)
                          (car ops)))
         (rest-operands (lambda (ops)
                          (<change>
                             ()
                             ops)
                          (cdr ops)))
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
                                     (error "Too many arguments supplied" vars vals)
                                     (error "Too few arguments supplied" vars vals)))))
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
                                                             (error "Unbound variable" var)
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
                                                           (error "Unbound variable -- SET!" var)
                                                           (let ((frame (first-frame env)))
                                                              (scan (frame-variables frame) (frame-values frame))))))))
                                   (env-loop env))))
         (define-variable! (lambda (var val env)
                             (<change>
                                ()
                                (display car))
                             (let ((frame (first-frame env)))
                                (letrec ((scan (lambda (vars vals)
                                                 (if (null? vars)
                                                    (add-binding-to-frame! var val frame)
                                                    (if (eq? var (car vars))
                                                       (<change>
                                                          (set-car! vals val)
                                                          (scan (cdr vars) (cdr vals)))
                                                       (<change>
                                                          (scan (cdr vars) (cdr vals))
                                                          (set-car! vals val)))))))
                                   (scan (frame-variables frame) (frame-values frame))))))
         (apply-primitive-procedure (lambda (proc args)
                                      (apply-in-underlying-scheme (primitive-implementation proc) args)))
         (compile (lambda (exp target linkage)
                    (if (self-evaluating? exp)
                       (compile-self-evaluating exp target linkage)
                       (if (quoted? exp)
                          (compile-quoted exp target linkage)
                          (if (variable? exp)
                             (compile-variable exp target linkage)
                             (if (assignment? exp)
                                (compile-assignment exp target linkage)
                                (if (definition? exp)
                                   (compile-definition exp target linkage)
                                   (if (if? exp)
                                      (compile-if exp target linkage)
                                      (if (lambda? exp)
                                         (compile-lambda exp target linkage)
                                         (if (begin? exp)
                                            (compile-sequence (begin-actions exp) target linkage)
                                            (if (cond? exp)
                                               (compile (cond->if exp) target linkage)
                                               (if (application? exp)
                                                  (compile-application exp target linkage)
                                                  (error "Unknown expression type -- COMPILE" exp)))))))))))))
         (make-instruction-sequence (lambda (needs modifies statements)
                                      (list needs modifies statements)))
         (empty-instruction-sequence (lambda ()
                                       (make-instruction-sequence () () ())))
         (compile-linkage (lambda (linkage)
                            (if (eq? linkage 'return)
                               (make-instruction-sequence
                                  (__toplevel_cons 'continue ())
                                  ()
                                  (__toplevel_cons
                                     (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                     ()))
                               (if (eq? linkage 'next)
                                  (empty-instruction-sequence)
                                  (make-instruction-sequence
                                     ()
                                     ()
                                     (__toplevel_cons
                                        (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons linkage ())) ()))
                                        ()))))))
         (end-with-linkage (lambda (linkage instruction-sequence)
                             (preserving (__toplevel_cons 'continue ()) instruction-sequence (compile-linkage linkage))))
         (compile-self-evaluating (lambda (exp target linkage)
                                    (end-with-linkage
                                       linkage
                                       (make-instruction-sequence
                                          ()
                                          (list target)
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'assign
                                                (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons exp ())) ())))
                                             ())))))
         (compile-quoted (lambda (exp target linkage)
                           (end-with-linkage
                              linkage
                              (make-instruction-sequence
                                 ()
                                 (list target)
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'assign
                                       (__toplevel_cons
                                          target
                                          (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons (text-of-quotation exp) ())) ())))
                                    ())))))
         (compile-variable (lambda (exp target linkage)
                             (end-with-linkage
                                linkage
                                (make-instruction-sequence
                                   (__toplevel_cons 'env ())
                                   (list target)
                                   (__toplevel_cons
                                      (__toplevel_cons
                                         'assign
                                         (__toplevel_cons
                                            target
                                            (__toplevel_cons
                                               (__toplevel_cons 'op (__toplevel_cons 'lookup-variable-value ()))
                                               (__toplevel_cons
                                                  (__toplevel_cons 'const (__toplevel_cons exp ()))
                                                  (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                      ())))))
         (compile-assignment (lambda (exp target linkage)
                               (let ((var (assignment-variable exp))
                                     (get-value-code (compile (assignment-value exp) 'val 'next)))
                                  (end-with-linkage
                                     linkage
                                     (preserving
                                        (__toplevel_cons 'env ())
                                        get-value-code
                                        (make-instruction-sequence
                                           (__toplevel_cons 'env (__toplevel_cons 'val ()))
                                           (list target)
                                           (__toplevel_cons
                                              (__toplevel_cons
                                                 'perform
                                                 (__toplevel_cons
                                                    (__toplevel_cons 'op (__toplevel_cons 'set-variable-value! ()))
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'const (__toplevel_cons var ()))
                                                       (__toplevel_cons
                                                          (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                          (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                              (__toplevel_cons
                                                 (__toplevel_cons
                                                    'assign
                                                    (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 'ok ())) ())))
                                                 ()))))))))
         (compile-definition (lambda (exp target linkage)
                               (let ((var (definition-variable exp))
                                     (get-value-code (compile (definition-value exp) 'val 'next)))
                                  (end-with-linkage
                                     linkage
                                     (preserving
                                        (__toplevel_cons 'env ())
                                        get-value-code
                                        (make-instruction-sequence
                                           (__toplevel_cons 'env (__toplevel_cons 'val ()))
                                           (list target)
                                           (__toplevel_cons
                                              (__toplevel_cons
                                                 'perform
                                                 (__toplevel_cons
                                                    (__toplevel_cons 'op (__toplevel_cons 'define-variable! ()))
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'const (__toplevel_cons var ()))
                                                       (__toplevel_cons
                                                          (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                          (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                              (__toplevel_cons
                                                 (__toplevel_cons
                                                    'assign
                                                    (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 'ok ())) ())))
                                                 ()))))))))
         (label-counter 0)
         (new-label-number (lambda ()
                             (set! label-counter (+ 1 label-counter))
                             label-counter))
         (make-label (lambda (name)
                       (string->symbol (string-append (symbol->string name) (number->string (new-label-number))))))
         (compile-if (lambda (exp target linkage)
                       (let ((t-branch (make-label 'true-branch))
                             (f-branch (make-label 'false-branch))
                             (after-if (make-label 'after-if)))
                          (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
                             (let ((p-code (compile (if-predicate exp) 'val 'next))
                                   (c-code (compile (if-consequent exp) target consequent-linkage))
                                   (a-code (compile (if-alternative exp) target linkage)))
                                (preserving
                                   (__toplevel_cons 'env (__toplevel_cons 'continue ()))
                                   p-code
                                   (append-instruction-sequences
                                      (make-instruction-sequence
                                         (__toplevel_cons 'val ())
                                         ()
                                         (__toplevel_cons
                                            (__toplevel_cons
                                               'test
                                               (__toplevel_cons
                                                  (__toplevel_cons 'op (__toplevel_cons 'false? ()))
                                                  (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))
                                            (__toplevel_cons
                                               (__toplevel_cons
                                                  'branch
                                                  (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons f-branch ())) ()))
                                               ())))
                                      (parallel-instruction-sequences
                                         (append-instruction-sequences t-branch c-code)
                                         (append-instruction-sequences f-branch a-code))
                                      after-if)))))))
         (compile-sequence (lambda (seq target linkage)
                             (if (last-exp? seq)
                                (compile (first-exp seq) target linkage)
                                (preserving
                                   (__toplevel_cons 'env (__toplevel_cons 'continue ()))
                                   (compile (first-exp seq) target 'next)
                                   (compile-sequence (rest-exps seq) target linkage)))))
         (compile-lambda (lambda (exp target linkage)
                           (let ((proc-entry (make-label 'entry))
                                 (after-lambda (make-label 'after-lambda)))
                              (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
                                 (append-instruction-sequences
                                    (tack-on-instruction-sequence
                                       (end-with-linkage
                                          lambda-linkage
                                          (make-instruction-sequence
                                             (__toplevel_cons 'env ())
                                             (list target)
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   'assign
                                                   (__toplevel_cons
                                                      target
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'op (__toplevel_cons 'make-compiled-procedure ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'label (__toplevel_cons proc-entry ()))
                                                            (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                                ())))
                                       (compile-lambda-body exp proc-entry))
                                    after-lambda)))))
         (compile-lambda-body (lambda (exp proc-entry)
                                (let ((formals (lambda-parameters exp)))
                                   (<change>
                                      (append-instruction-sequences
                                         (make-instruction-sequence
                                            (__toplevel_cons 'env (__toplevel_cons 'proc (__toplevel_cons 'argl ())))
                                            (__toplevel_cons 'env ())
                                            (__toplevel_cons
                                               proc-entry
                                               (__toplevel_cons
                                                  (__toplevel_cons
                                                     'assign
                                                     (__toplevel_cons
                                                        'env
                                                        (__toplevel_cons
                                                           (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-env ()))
                                                           (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                                                  (__toplevel_cons
                                                     (__toplevel_cons
                                                        'assign
                                                        (__toplevel_cons
                                                           'env
                                                           (__toplevel_cons
                                                              (__toplevel_cons 'op (__toplevel_cons 'extend-environment ()))
                                                              (__toplevel_cons
                                                                 (__toplevel_cons 'const (__toplevel_cons formals ()))
                                                                 (__toplevel_cons
                                                                    (__toplevel_cons 'reg (__toplevel_cons 'argl ()))
                                                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ()))))))
                                                     ()))))
                                         (compile-sequence (lambda-body exp) 'val 'return))
                                      ((lambda (x) x)
                                         (append-instruction-sequences
                                            (make-instruction-sequence
                                               (__toplevel_cons 'env (__toplevel_cons 'proc (__toplevel_cons 'argl ())))
                                               (__toplevel_cons 'env ())
                                               (__toplevel_cons
                                                  proc-entry
                                                  (__toplevel_cons
                                                     (__toplevel_cons
                                                        'assign
                                                        (__toplevel_cons
                                                           'env
                                                           (__toplevel_cons
                                                              (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-env ()))
                                                              (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                                                     (__toplevel_cons
                                                        (__toplevel_cons
                                                           'assign
                                                           (__toplevel_cons
                                                              'env
                                                              (__toplevel_cons
                                                                 (__toplevel_cons 'op (__toplevel_cons 'extend-environment ()))
                                                                 (__toplevel_cons
                                                                    (__toplevel_cons 'const (__toplevel_cons formals ()))
                                                                    (__toplevel_cons
                                                                       (__toplevel_cons 'reg (__toplevel_cons 'argl ()))
                                                                       (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ()))))))
                                                        ()))))
                                            (compile-sequence (lambda-body exp) 'val 'return)))))))
         (compile-application (lambda (exp target linkage)
                                (let ((proc-code (compile (operator exp) 'proc 'next))
                                      (operand-codes (map
                                                       (lambda (operand)
                                                          (<change>
                                                             (compile operand 'val 'next)
                                                             ((lambda (x) x) (compile operand 'val 'next))))
                                                       (operands exp))))
                                   (preserving
                                      (__toplevel_cons 'env (__toplevel_cons 'continue ()))
                                      proc-code
                                      (preserving
                                         (__toplevel_cons 'proc (__toplevel_cons 'continue ()))
                                         (construct-arglist operand-codes)
                                         (compile-procedure-call target linkage))))))
         (construct-arglist (lambda (operand-codes)
                              (let ((operand-codes (reverse operand-codes)))
                                 (if (null? operand-codes)
                                    (make-instruction-sequence
                                       ()
                                       (__toplevel_cons 'argl ())
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'assign
                                             (__toplevel_cons 'argl (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons () ())) ())))
                                          ()))
                                    (let ((code-to-get-last-arg (append-instruction-sequences
                                                                  (car operand-codes)
                                                                  (make-instruction-sequence
                                                                     (__toplevel_cons 'val ())
                                                                     (__toplevel_cons 'argl ())
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons
                                                                           'assign
                                                                           (__toplevel_cons
                                                                              'argl
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'op (__toplevel_cons 'list ()))
                                                                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))))
                                                                        ())))))
                                       (if (null? (cdr operand-codes))
                                          code-to-get-last-arg
                                          (preserving
                                             (__toplevel_cons 'env ())
                                             code-to-get-last-arg
                                             (code-to-get-rest-args (cdr operand-codes)))))))))
         (code-to-get-rest-args (lambda (operand-codes)
                                  (let ((code-for-next-arg (preserving
                                                             (__toplevel_cons 'argl ())
                                                             (car operand-codes)
                                                             (make-instruction-sequence
                                                                (__toplevel_cons 'val (__toplevel_cons 'argl ()))
                                                                (__toplevel_cons 'argl ())
                                                                (__toplevel_cons
                                                                   (__toplevel_cons
                                                                      'assign
                                                                      (__toplevel_cons
                                                                         'argl
                                                                         (__toplevel_cons
                                                                            (__toplevel_cons 'op (__toplevel_cons 'cons ()))
                                                                            (__toplevel_cons
                                                                               (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                                               (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                                                   ())))))
                                     (if (null? (cdr operand-codes))
                                        code-for-next-arg
                                        (preserving
                                           (__toplevel_cons 'env ())
                                           code-for-next-arg
                                           (code-to-get-rest-args (cdr operand-codes)))))))
         (compile-procedure-call (lambda (target linkage)
                                   (let ((primitive-branch (make-label 'primitive-branch))
                                         (compiled-branch (make-label 'compiled-branch))
                                         (after-call (make-label 'after-call)))
                                      (<change>
                                         (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
                                            (append-instruction-sequences
                                               (make-instruction-sequence
                                                  (__toplevel_cons 'proc ())
                                                  ()
                                                  (__toplevel_cons
                                                     (__toplevel_cons
                                                        'test
                                                        (__toplevel_cons
                                                           (__toplevel_cons 'op (__toplevel_cons 'primitive-procedure? ()))
                                                           (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ())))
                                                     (__toplevel_cons
                                                        (__toplevel_cons
                                                           'branch
                                                           (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons primitive-branch ())) ()))
                                                        ())))
                                               (parallel-instruction-sequences
                                                  (append-instruction-sequences compiled-branch (compile-proc-appl target compiled-linkage))
                                                  (append-instruction-sequences
                                                     primitive-branch
                                                     (end-with-linkage
                                                        linkage
                                                        (make-instruction-sequence
                                                           (__toplevel_cons 'proc (__toplevel_cons 'argl ()))
                                                           (list target)
                                                           (__toplevel_cons
                                                              (__toplevel_cons
                                                                 'assign
                                                                 (__toplevel_cons
                                                                    target
                                                                    (__toplevel_cons
                                                                       (__toplevel_cons 'op (__toplevel_cons 'apply-primitive-procedure ()))
                                                                       (__toplevel_cons
                                                                          (__toplevel_cons 'reg (__toplevel_cons 'proc ()))
                                                                          (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                                              ())))))
                                               after-call))
                                         ((lambda (x) x)
                                            (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
                                               (append-instruction-sequences
                                                  (make-instruction-sequence
                                                     (__toplevel_cons 'proc ())
                                                     ()
                                                     (__toplevel_cons
                                                        (__toplevel_cons
                                                           'test
                                                           (__toplevel_cons
                                                              (__toplevel_cons 'op (__toplevel_cons 'primitive-procedure? ()))
                                                              (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ())))
                                                        (__toplevel_cons
                                                           (__toplevel_cons
                                                              'branch
                                                              (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons primitive-branch ())) ()))
                                                           ())))
                                                  (parallel-instruction-sequences
                                                     (append-instruction-sequences compiled-branch (compile-proc-appl target compiled-linkage))
                                                     (append-instruction-sequences
                                                        primitive-branch
                                                        (end-with-linkage
                                                           linkage
                                                           (make-instruction-sequence
                                                              (__toplevel_cons 'proc (__toplevel_cons 'argl ()))
                                                              (list target)
                                                              (__toplevel_cons
                                                                 (__toplevel_cons
                                                                    'assign
                                                                    (__toplevel_cons
                                                                       target
                                                                       (__toplevel_cons
                                                                          (__toplevel_cons 'op (__toplevel_cons 'apply-primitive-procedure ()))
                                                                          (__toplevel_cons
                                                                             (__toplevel_cons 'reg (__toplevel_cons 'proc ()))
                                                                             (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                                                 ())))))
                                                  after-call)))))))
         (compile-proc-appl (lambda (target linkage)
                              (if (if (eq? target 'val) (not (eq? linkage 'return)) #f)
                                 (make-instruction-sequence
                                    (__toplevel_cons 'proc ())
                                    all-regs
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'assign
                                          (__toplevel_cons
                                             'continue
                                             (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons linkage ())) ())))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'assign
                                             (__toplevel_cons
                                                'val
                                                (__toplevel_cons
                                                   (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                                                   (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                                          (__toplevel_cons
                                             (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                                             ()))))
                                 (if (if (<change> (not (eq? target 'val)) (not (not (eq? target 'val)))) (not (eq? linkage 'return)) #f)
                                    (let ((proc-return (make-label 'proc-return)))
                                       (make-instruction-sequence
                                          (__toplevel_cons 'proc ())
                                          all-regs
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'assign
                                                (__toplevel_cons
                                                   'continue
                                                   (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons proc-return ())) ())))
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   'assign
                                                   (__toplevel_cons
                                                      'val
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                                                         (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                                                (__toplevel_cons
                                                   (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                                                   (__toplevel_cons
                                                      proc-return
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'assign
                                                            (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons linkage ())) ()))
                                                            ()))))))))
                                    (if (if (eq? target 'val) (eq? linkage 'return) #f)
                                       (make-instruction-sequence
                                          (__toplevel_cons 'proc (__toplevel_cons 'continue ()))
                                          all-regs
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'assign
                                                (__toplevel_cons
                                                   'val
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                                                      (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                                             (__toplevel_cons
                                                (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                                                ())))
                                       (if (<change> (if (not (eq? target 'val)) (eq? linkage 'return) #f) (not (if (not (eq? target 'val)) (eq? linkage 'return) #f)))
                                          (error "return linkage, target not val -- COMPILE" target)
                                          #f))))))
         (all-regs (__toplevel_cons
                     'env
                     (__toplevel_cons
                        'proc
                        (__toplevel_cons 'val (__toplevel_cons 'argl (__toplevel_cons 'continue ()))))))
         (registers-needed (lambda (s)
                             (if (symbol? s) () (car s))))
         (registers-modified (lambda (s)
                               (<change>
                                  ()
                                  (symbol? s))
                               (<change>
                                  (if (symbol? s) () (cadr s))
                                  ((lambda (x) x) (if (symbol? s) () (cadr s))))))
         (statements (lambda (s)
                       (if (symbol? s) (list s) (caddr s))))
         (needs-register? (lambda (seq reg)
                            (<change>
                               ()
                               reg)
                            (memq reg (registers-needed seq))))
         (modifies-register? (lambda (seq reg)
                               (memq reg (registers-modified seq))))
         (append-instruction-sequences (lambda seqs
                                         (letrec ((append-2-sequences (lambda (seq1 seq2)
                                                                        (make-instruction-sequence
                                                                           (list-union
                                                                              (registers-needed seq1)
                                                                              (list-difference (registers-needed seq2) (registers-modified seq1)))
                                                                           (list-union (registers-modified seq1) (registers-modified seq2))
                                                                           (append (statements seq1) (statements seq2)))))
                                                  (append-seq-list (lambda (seqs)
                                                                     (if (null? seqs)
                                                                        (empty-instruction-sequence)
                                                                        (append-2-sequences (car seqs) (append-seq-list (cdr seqs)))))))
                                            (append-seq-list seqs))))
         (list-union (lambda (s1 s2)
                       (if (null? s1)
                          s2
                          (if (memq (car s1) s2)
                             (list-union (cdr s1) s2)
                             (cons (car s1) (list-union (cdr s1) s2))))))
         (list-difference (lambda (s1 s2)
                            (if (null? s1)
                               ()
                               (if (memq (car s1) s2)
                                  (list-difference (cdr s1) s2)
                                  (cons (car s1) (list-difference (cdr s1) s2))))))
         (preserving (lambda (regs seq1 seq2)
                       (if (null? regs)
                          (append-instruction-sequences seq1 seq2)
                          (let ((first-reg (car regs)))
                             (if (if (needs-register? seq2 first-reg) (modifies-register? seq1 first-reg) #f)
                                (preserving
                                   (cdr regs)
                                   (make-instruction-sequence
                                      (list-union (list first-reg) (registers-needed seq1))
                                      (list-difference (registers-modified seq1) (list first-reg))
                                      (append
                                         (__toplevel_cons (__toplevel_cons 'save (__toplevel_cons first-reg ())) ())
                                         (append
                                            (statements seq1)
                                            (__toplevel_cons (__toplevel_cons 'restore (__toplevel_cons first-reg ())) ()))))
                                   seq2)
                                (preserving (cdr regs) seq1 seq2))))))
         (tack-on-instruction-sequence (lambda (seq body-seq)
                                         (make-instruction-sequence
                                            (registers-needed seq)
                                            (registers-modified seq)
                                            (append (statements seq) (statements body-seq)))))
         (parallel-instruction-sequences (lambda (seq1 seq2)
                                           (make-instruction-sequence
                                              (list-union (registers-needed seq1) (registers-needed seq2))
                                              (list-union (registers-modified seq1) (registers-modified seq2))
                                              (append (statements seq1) (statements seq2)))))
         (make-compiled-procedure (lambda (entry env)
                                    (list 'compiled-procedure entry env)))
         (compiled-procedure? (lambda (proc)
                                (tagged-list? proc 'compiled-procedure)))
         (compiled-procedure-entry (lambda (c-proc)
                                     (cadr c-proc)))
         (compiled-procedure-env (lambda (c-proc)
                                   (caddr c-proc))))
   (compile
      (__toplevel_cons
         'begin
         (__toplevel_cons
            (__toplevel_cons
               'define
               (__toplevel_cons (__toplevel_cons 'id (__toplevel_cons 'x ())) (__toplevel_cons 'x ())))
            (__toplevel_cons (__toplevel_cons 'id (__toplevel_cons 2 ())) ())))
      'val
      'next)
   (compile
      (__toplevel_cons
         'begin
         (__toplevel_cons
            (__toplevel_cons
               'define
               (__toplevel_cons
                  (__toplevel_cons 'fac (__toplevel_cons 'n ()))
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
                                             'fac
                                             (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'n (__toplevel_cons 1 ()))) ()))
                                          ())))
                                 ()))))
                     ())))
            (__toplevel_cons (__toplevel_cons 'fac (__toplevel_cons 5 ())) ())))
      'val
      'next))