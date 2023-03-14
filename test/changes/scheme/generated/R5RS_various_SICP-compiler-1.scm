; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 3
; * swapped branches: 1
; * calls to id fun: 9
(letrec ((true #t)
         (false #f)
         (self-evaluating? (lambda (exp)
                             (if (number? exp)
                                true
                                (if (string? exp) true false))))
         (quoted? (lambda (exp)
                    (tagged-list? exp 'quote)))
         (text-of-quotation (lambda (exp)
                              (cadr exp)))
         (tagged-list? (lambda (exp tag)
                         (if (pair? exp) (eq? (car exp) tag) false)))
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
         (lambda? (lambda (exp)
                    (tagged-list? exp 'lambda)))
         (lambda-parameters (lambda (exp)
                              (cadr exp)))
         (lambda-body (lambda (exp)
                        (cddr exp)))
         (make-lambda (lambda (parameters body)
                        (cons 'lambda (cons parameters body))))
         (if? (lambda (exp)
                (tagged-list? exp 'if)))
         (if-predicate (lambda (exp)
                         (cadr exp)))
         (if-consequent (lambda (exp)
                          (<change>
                             (caddr exp)
                             ((lambda (x) x) (caddr exp)))))
         (if-alternative (lambda (exp)
                           (if (not (null? (cdddr exp)))
                              (cadddr exp)
                              'false)))
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
                          (cdr ops)))
         (make-if (lambda (predicate consequent alternative)
                    (cons 'if (cons predicate (cons consequent (cons alternative ()))))))
         (sequence->exp (lambda (seq)
                          (if (null? seq)
                             seq
                             (if (last-exp? seq)
                                (first-exp seq)
                                (make-begin seq)))))
         (make-begin (lambda (seq)
                       (cons 'begin seq)))
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
                     (expand-clauses (cond-clauses exp))))
         (expand-clauses (lambda (clauses)
                           (if (null? clauses)
                              'false
                              (let ((first (car clauses))
                                    (rest (cdr clauses)))
                                 (if (cond-else-clause? first)
                                    (if (<change> (null? rest) (not (null? rest)))
                                       (sequence->exp (cond-actions first))
                                       (error "ELSE clause isn't last -- COND->IF" clauses))
                                    (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))))
         (compile (lambda (exp target linkage)
                    (<change>
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
                                                     (error "Unknown expression type -- COMPILE" exp)))))))))))
                       ((lambda (x) x)
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
                                                  (<change>
                                                     (compile-sequence (begin-actions exp) target linkage)
                                                     (if (cond? exp)
                                                        (compile (cond->if exp) target linkage)
                                                        (if (application? exp)
                                                           (compile-application exp target linkage)
                                                           (error "Unknown expression type -- COMPILE" exp))))
                                                  (<change>
                                                     (if (cond? exp)
                                                        (compile (cond->if exp) target linkage)
                                                        (if (application? exp)
                                                           (compile-application exp target linkage)
                                                           (error "Unknown expression type -- COMPILE" exp)))
                                                     (compile-sequence (begin-actions exp) target linkage))))))))))))))
         (make-instruction-sequence (lambda (needs modifies statements)
                                      (cons needs (cons modifies (cons statements ())))))
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
                                          (cons target ())
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
                                 (cons target ())
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
                                   (cons target ())
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
                                  (<change>
                                     (end-with-linkage
                                        linkage
                                        (preserving
                                           (__toplevel_cons 'env ())
                                           get-value-code
                                           (make-instruction-sequence
                                              (__toplevel_cons 'env (__toplevel_cons 'val ()))
                                              (cons target ())
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
                                                    ())))))
                                     ((lambda (x) x)
                                        (end-with-linkage
                                           linkage
                                           (preserving
                                              (__toplevel_cons 'env ())
                                              get-value-code
                                              (make-instruction-sequence
                                                 (__toplevel_cons 'env (__toplevel_cons 'val ()))
                                                 (cons target ())
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
                                                       ()))))))))))
         (compile-definition (lambda (exp target linkage)
                               (let ((var (definition-variable exp))
                                     (get-value-code (compile (definition-value exp) 'val 'next)))
                                  (<change>
                                     (end-with-linkage
                                        linkage
                                        (preserving
                                           (__toplevel_cons 'env ())
                                           get-value-code
                                           (make-instruction-sequence
                                              (__toplevel_cons 'env (__toplevel_cons 'val ()))
                                              (cons target ())
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
                                                    ())))))
                                     ((lambda (x) x)
                                        (end-with-linkage
                                           linkage
                                           (preserving
                                              (__toplevel_cons 'env ())
                                              get-value-code
                                              (make-instruction-sequence
                                                 (__toplevel_cons 'env (__toplevel_cons 'val ()))
                                                 (cons target ())
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
                                                       ()))))))))))
         (label-counter 0)
         (new-label-number (lambda ()
                             (<change>
                                (set! label-counter (+ 1 label-counter))
                                ((lambda (x) x) (set! label-counter (+ 1 label-counter))))
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
                                      (cons
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
                                         (cons
                                            (parallel-instruction-sequences
                                               (append-instruction-sequences t-branch c-code)
                                               (append-instruction-sequences f-branch a-code))
                                            (cons after-if ()))))))))))
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
                                    (cons
                                       (tack-on-instruction-sequence
                                          (end-with-linkage
                                             lambda-linkage
                                             (make-instruction-sequence
                                                (__toplevel_cons 'env ())
                                                (cons target ())
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
                                       (cons after-lambda ())))))))
         (compile-lambda-body (lambda (exp proc-entry)
                                (let ((formals (lambda-parameters exp)))
                                   (<change>
                                      (append-instruction-sequences
                                         (cons
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
                                            (cons (compile-sequence (lambda-body exp) 'val 'return) ())))
                                      ((lambda (x) x)
                                         (append-instruction-sequences
                                            (cons
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
                                               (cons (compile-sequence (lambda-body exp) 'val 'return) ()))))))))
         (compile-application (lambda (exp target linkage)
                                (let ((proc-code (compile (operator exp) 'proc 'next))
                                      (operand-codes (map (lambda (operand) (compile operand 'val 'next)) (operands exp))))
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
                                                                  (cons
                                                                     (car operand-codes)
                                                                     (cons
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
                                                                              ()))
                                                                        ())))))
                                       (if (null? (cdr operand-codes))
                                          code-to-get-last-arg
                                          (preserving
                                             (__toplevel_cons 'env ())
                                             code-to-get-last-arg
                                             (code-to-get-rest-args (cdr operand-codes)))))))))
         (code-to-get-rest-args (lambda (operand-codes)
                                  (<change>
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
                                              (code-to-get-rest-args (cdr operand-codes)))))
                                     ((lambda (x) x)
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
                                                 (code-to-get-rest-args (cdr operand-codes)))))))))
         (compile-procedure-call (lambda (target linkage)
                                   (let ((primitive-branch (make-label 'primitive-branch))
                                         (compiled-branch (make-label 'compiled-branch))
                                         (after-call (make-label 'after-call)))
                                      (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
                                         (append-instruction-sequences
                                            (cons
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
                                               (cons
                                                  (parallel-instruction-sequences
                                                     (append-instruction-sequences
                                                        (cons compiled-branch (cons (compile-proc-appl target compiled-linkage) ())))
                                                     (append-instruction-sequences
                                                        (cons
                                                           primitive-branch
                                                           (cons
                                                              (end-with-linkage
                                                                 linkage
                                                                 (make-instruction-sequence
                                                                    (__toplevel_cons 'proc (__toplevel_cons 'argl ()))
                                                                    (cons target ())
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
                                                                       ())))
                                                              ()))))
                                                  (cons after-call ()))))))))
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
                                 (if (if (not (eq? target 'val)) (not (eq? linkage 'return)) #f)
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
                                       (if (if (not (eq? target 'val)) (eq? linkage 'return) #f)
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
                               (if (symbol? s) () (cadr s))))
         (statements (lambda (s)
                       (if (symbol? s) (cons s ()) (caddr s))))
         (needs-register? (lambda (seq reg)
                            (memq reg (registers-needed seq))))
         (modifies-register? (lambda (seq reg)
                               (memq reg (registers-modified seq))))
         (append-instruction-sequences (lambda (seqs)
                                         (<change>
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
                                               (append-seq-list seqs))
                                            ((lambda (x) x)
                                               (letrec ((append-2-sequences (lambda (seq1 seq2)
                                                                              (make-instruction-sequence
                                                                                 (list-union
                                                                                    (registers-needed seq1)
                                                                                    (list-difference (registers-needed seq2) (registers-modified seq1)))
                                                                                 (list-union (registers-modified seq1) (registers-modified seq2))
                                                                                 (append (statements seq1) (statements seq2)))))
                                                        (append-seq-list (lambda (seqs)
                                                                           (if (<change> (null? seqs) (not (null? seqs)))
                                                                              (empty-instruction-sequence)
                                                                              (append-2-sequences (car seqs) (append-seq-list (cdr seqs)))))))
                                                  (append-seq-list seqs))))))
         (list-union (lambda (s1 s2)
                       (if (null? s1)
                          s2
                          (if (<change> (memq (car s1) s2) (not (memq (car s1) s2)))
                             (list-union (cdr s1) s2)
                             (cons (car s1) (list-union (cdr s1) s2))))))
         (list-difference (lambda (s1 s2)
                            (if (null? s1)
                               ()
                               (if (memq (car s1) s2)
                                  (list-difference (cdr s1) s2)
                                  (cons (car s1) (list-difference (cdr s1) s2))))))
         (preserving (lambda (regs seq1 seq2)
                       (<change>
                          ()
                          registers-modified)
                       (if (null? regs)
                          (append-instruction-sequences (cons seq1 (cons seq2 ())))
                          (let ((first-reg (car regs)))
                             (if (if (needs-register? seq2 first-reg) (modifies-register? seq1 first-reg) #f)
                                (preserving
                                   (cdr regs)
                                   (make-instruction-sequence
                                      (list-union (cons first-reg ()) (registers-needed seq1))
                                      (list-difference (registers-modified seq1) (cons first-reg ()))
                                      (append
                                         (__toplevel_cons (__toplevel_cons 'save (__toplevel_cons first-reg ())) ())
                                         (statements seq1)
                                         (__toplevel_cons (__toplevel_cons 'restore (__toplevel_cons first-reg ())) ())))
                                   seq2)
                                (preserving (cdr regs) seq1 seq2))))))
         (tack-on-instruction-sequence (lambda (seq body-seq)
                                         (<change>
                                            ()
                                            seq)
                                         (make-instruction-sequence
                                            (registers-needed seq)
                                            (registers-modified seq)
                                            (append (statements seq) (statements body-seq)))))
         (parallel-instruction-sequences (lambda (seq1 seq2)
                                           (make-instruction-sequence
                                              (list-union (registers-needed seq1) (registers-needed seq2))
                                              (list-union (registers-modified seq1) (registers-modified seq2))
                                              (append (statements seq1) (statements seq2))))))
   (<change>
      ()
      (display ()))
   (let ((result1 (compile (__toplevel_cons 'define (__toplevel_cons 'x (__toplevel_cons 4 ()))) 'val 'return))
         (expected-result1 (__toplevel_cons
                             (__toplevel_cons 'env (__toplevel_cons 'continue ()))
                             (__toplevel_cons
                                (__toplevel_cons 'val ())
                                (__toplevel_cons
                                   (__toplevel_cons
                                      (__toplevel_cons
                                         'assign
                                         (__toplevel_cons 'val (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 4 ())) ())))
                                      (__toplevel_cons
                                         (__toplevel_cons
                                            'perform
                                            (__toplevel_cons
                                               (__toplevel_cons 'op (__toplevel_cons 'define-variable! ()))
                                               (__toplevel_cons
                                                  (__toplevel_cons 'const (__toplevel_cons 'x ()))
                                                  (__toplevel_cons
                                                     (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                     (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                         (__toplevel_cons
                                            (__toplevel_cons
                                               'assign
                                               (__toplevel_cons 'val (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 'ok ())) ())))
                                            (__toplevel_cons
                                               (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                               ()))))
                                   ()))))
         (result2 (compile
                    (__toplevel_cons
                       'lambda
                       (__toplevel_cons
                          (__toplevel_cons 'x (__toplevel_cons 'y ()))
                          (__toplevel_cons (__toplevel_cons '* (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                    'next
                    'val))
         (expected-result2 (__toplevel_cons
                             (__toplevel_cons 'env ())
                             (__toplevel_cons
                                (__toplevel_cons 'next ())
                                (__toplevel_cons
                                   (__toplevel_cons
                                      (__toplevel_cons
                                         'assign
                                         (__toplevel_cons
                                            'next
                                            (__toplevel_cons
                                               (__toplevel_cons 'op (__toplevel_cons 'make-compiled-procedure ()))
                                               (__toplevel_cons
                                                  (__toplevel_cons 'label (__toplevel_cons 'entry1 ()))
                                                  (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                      (__toplevel_cons
                                         (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'val ())) ()))
                                         (__toplevel_cons
                                            'entry1
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
                                                              (__toplevel_cons 'const (__toplevel_cons (__toplevel_cons 'x (__toplevel_cons 'y ())) ()))
                                                              (__toplevel_cons
                                                                 (__toplevel_cons 'reg (__toplevel_cons 'argl ()))
                                                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ()))))))
                                                  (__toplevel_cons
                                                     (__toplevel_cons
                                                        'assign
                                                        (__toplevel_cons
                                                           'proc
                                                           (__toplevel_cons
                                                              (__toplevel_cons 'op (__toplevel_cons 'lookup-variable-value ()))
                                                              (__toplevel_cons
                                                                 (__toplevel_cons 'const (__toplevel_cons '* ()))
                                                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                                     (__toplevel_cons
                                                        (__toplevel_cons
                                                           'assign
                                                           (__toplevel_cons
                                                              'val
                                                              (__toplevel_cons
                                                                 (__toplevel_cons 'op (__toplevel_cons 'lookup-variable-value ()))
                                                                 (__toplevel_cons
                                                                    (__toplevel_cons 'const (__toplevel_cons 'y ()))
                                                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                                        (__toplevel_cons
                                                           (__toplevel_cons
                                                              'assign
                                                              (__toplevel_cons
                                                                 'argl
                                                                 (__toplevel_cons
                                                                    (__toplevel_cons 'op (__toplevel_cons 'list ()))
                                                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))))
                                                           (__toplevel_cons
                                                              (__toplevel_cons
                                                                 'assign
                                                                 (__toplevel_cons
                                                                    'val
                                                                    (__toplevel_cons
                                                                       (__toplevel_cons 'op (__toplevel_cons 'lookup-variable-value ()))
                                                                       (__toplevel_cons
                                                                          (__toplevel_cons 'const (__toplevel_cons 'x ()))
                                                                          (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
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
                                                                 (__toplevel_cons
                                                                    (__toplevel_cons
                                                                       'test
                                                                       (__toplevel_cons
                                                                          (__toplevel_cons 'op (__toplevel_cons 'primitive-procedure? ()))
                                                                          (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ())))
                                                                    (__toplevel_cons
                                                                       (__toplevel_cons
                                                                          'branch
                                                                          (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'primitive-branch3 ())) ()))
                                                                       (__toplevel_cons
                                                                          'compiled-branch4
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
                                                                                   'primitive-branch3
                                                                                   (__toplevel_cons
                                                                                      (__toplevel_cons
                                                                                         'assign
                                                                                         (__toplevel_cons
                                                                                            'val
                                                                                            (__toplevel_cons
                                                                                               (__toplevel_cons 'op (__toplevel_cons 'apply-primitive-procedure ()))
                                                                                               (__toplevel_cons
                                                                                                  (__toplevel_cons 'reg (__toplevel_cons 'proc ()))
                                                                                                  (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                                                                      (__toplevel_cons
                                                                                         (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                                         (__toplevel_cons 'after-call5 (__toplevel_cons 'after-lambda2 ()))))))))))))))))))))
                                   ())))))
      (if (equal? result1 expected-result1)
         (equal? result2 expected-result2)
         #f)))