; Changes:
; * removed: 3
; * added: 7
; * swaps: 2
; * negated predicates: 8
; * swapped branches: 8
; * calls to id fun: 13
(letrec ((void (lambda ()
                 (if #f #t #f)))
         (tagged-list? (lambda (tag l)
                         (<change>
                            (if (pair? l) (eq? tag (car l)) #f)
                            ((lambda (x) x) (if (pair? l) (<change> (eq? tag (car l)) #f) (<change> #f (eq? tag (car l))))))))
         (char->natural (lambda (c)
                          (let ((i (char->integer c)))
                             (if (< i 0) (* -2 i) (+ (* 2 i) 1)))))
         (integer->char-list (lambda (n)
                               (string->list (number->string n))))
         (gensym-count 0)
         (gensym (lambda (param)
                   (set! gensym-count (+ gensym-count 1))
                   (string->symbol
                      (string-append (if (symbol? param) (symbol->string param) param) "$" (number->string gensym-count)))))
         (symbol<? (lambda (sym1 sym2)
                     (string<? (symbol->string sym1) (symbol->string sym2))))
         (insert (lambda (sym S)
                   (if (not (pair? S))
                      (list sym)
                      (if (eq? sym (car S))
                         S
                         (if (<change> (symbol<? sym (car S)) (not (symbol<? sym (car S))))
                            (cons sym S)
                            (cons (car S) (insert sym (cdr S))))))))
         (remove (lambda (sym S)
                   (if (not (pair? S))
                      ()
                      (if (eq? (car S) sym)
                         (cdr S)
                         (cons (car S) (remove sym (cdr S)))))))
         (union (lambda (set1 set2)
                  (if (not (pair? set1))
                     set2
                     (insert (car set1) (union (cdr set1) set2)))))
         (difference (lambda (set1 set2)
                       (if (not (pair? set2))
                          set1
                          (difference (remove (car set2) set1) (cdr set2)))))
         (reduce (lambda (f lst init)
                   (if (not (pair? lst))
                      init
                      (reduce f (cdr lst) (f (car lst) init)))))
         (azip (lambda (list1 list2)
                 (if (if (pair? list1) (pair? list2) #f)
                    (cons (list (car list1) (car list2)) (azip (cdr list1) (cdr list2)))
                    ())))
         (assq-remove-key (lambda (env key)
                            (if (not (pair? env))
                               ()
                               (if (eq? (car (car env)) key)
                                  (assq-remove-key (cdr env) key)
                                  (cons (car env) (assq-remove-key (cdr env) key))))))
         (assq-remove-keys (lambda (env keys)
                             (if (not (pair? keys))
                                env
                                (assq-remove-keys (assq-remove-key env (car keys)) (cdr keys)))))
         (const? (lambda (exp)
                   (<change>
                      ()
                      boolean?)
                   (<change>
                      ()
                      boolean?)
                   (let ((__or_res (integer? exp)))
                      (if __or_res __or_res (boolean? exp)))))
         (ref? (lambda (exp)
                 (symbol? exp)))
         (let? (lambda (exp)
                 (tagged-list? 'let exp)))
         (let->bindings (lambda (exp)
                          (cadr exp)))
         (let->exp (lambda (exp)
                     (caddr exp)))
         (let->bound-vars (lambda (exp)
                            (map car (cadr exp))))
         (let->args (lambda (exp)
                      (map cadr (cadr exp))))
         (letrec? (lambda (exp)
                    (tagged-list? 'letrec exp)))
         (letrec->bindings (lambda (exp)
                             (cadr exp)))
         (letrec->exp (lambda (exp)
                        (caddr exp)))
         (letrec->bound-vars (lambda (exp)
                               (map car (cadr exp))))
         (letrec->args (lambda (exp)
                         (<change>
                            (map cadr (cadr exp))
                            ((lambda (x) x) (map cadr (cadr exp))))))
         (lambda? (lambda (exp)
                    (<change>
                       (tagged-list? 'lambda exp)
                       ((lambda (x) x) (tagged-list? 'lambda exp)))))
         (lambda->formals (lambda (exp)
                            (cadr exp)))
         (lambda->exp (lambda (exp)
                        (caddr exp)))
         (if? (lambda (exp)
                (tagged-list? 'if exp)))
         (if->condition (lambda (exp)
                          (cadr exp)))
         (if->then (lambda (exp)
                     (caddr exp)))
         (if->else (lambda (exp)
                     (cadddr exp)))
         (app? (lambda (exp)
                 (pair? exp)))
         (app->fun (lambda (exp)
                     (<change>
                        ()
                        (display exp))
                     (car exp)))
         (app->args (lambda (exp)
                      (<change>
                         (cdr exp)
                         ((lambda (x) x) (cdr exp)))))
         (prim? (lambda (exp)
                  (let ((__or_res (eq? exp '+)))
                     (if __or_res
                        __or_res
                        (let ((__or_res (eq? exp '-)))
                           (if __or_res
                              __or_res
                              (let ((__or_res (eq? exp '*)))
                                 (if __or_res
                                    __or_res
                                    (let ((__or_res (eq? exp '=)))
                                       (if __or_res __or_res (eq? exp 'display)))))))))))
         (begin? (lambda (exp)
                   (tagged-list? 'begin exp)))
         (begin->exps (lambda (exp)
                        (cdr exp)))
         (set!? (lambda (exp)
                  (tagged-list? 'set! exp)))
         (set!->var (lambda (exp)
                      (cadr exp)))
         (set!->exp (lambda (exp)
                      (caddr exp)))
         (closure? (lambda (exp)
                     (tagged-list? 'closure exp)))
         (closure->lam (lambda (exp)
                         (cadr exp)))
         (closure->env (lambda (exp)
                         (caddr exp)))
         (env-make? (lambda (exp)
                      (tagged-list? 'env-make exp)))
         (env-make->id (lambda (exp)
                         (cadr exp)))
         (env-make->fields (lambda (exp)
                             (map car (cddr exp))))
         (env-make->values (lambda (exp)
                             (map cadr (cddr exp))))
         (env-get? (lambda (exp)
                     (tagged-list? 'env-get exp)))
         (env-get->id (lambda (exp)
                        (cadr exp)))
         (env-get->field (lambda (exp)
                           (caddr exp)))
         (env-get->env (lambda (exp)
                         (<change>
                            ()
                            (cadddr exp))
                         (cadddr exp)))
         (set-cell!? (lambda (exp)
                       (tagged-list? 'set-cell! exp)))
         (set-cell!->cell (lambda (exp)
                            (cadr exp)))
         (set-cell!->value (lambda (exp)
                             (caddr exp)))
         (cell? (lambda (exp)
                  (<change>
                     (tagged-list? 'cell exp)
                     ((lambda (x) x) (tagged-list? 'cell exp)))))
         (cell->value (lambda (exp)
                        (cadr exp)))
         (cell-get? (lambda (exp)
                      (tagged-list? 'cell-get exp)))
         (cell-get->cell (lambda (exp)
                           (cadr exp)))
         (substitute-var (lambda (env var)
                           (let ((sub (assq var env)))
                              (if sub (cadr sub) var))))
         (substitute (lambda (env exp)
                       (letrec ((substitute-with (lambda (env)
                                                   (lambda (exp)
                                                      (<change>
                                                         (substitute env exp)
                                                         ((lambda (x) x) (substitute env exp)))))))
                          (if (null? env)
                             exp
                             (if (const? exp)
                                exp
                                (if (prim? exp)
                                   exp
                                   (if (ref? exp)
                                      (substitute-var env exp)
                                      (if (lambda? exp)
                                         (__toplevel_cons
                                            'lambda
                                            (__toplevel_cons
                                               (lambda->formals exp)
                                               (__toplevel_cons (substitute (assq-remove-keys env (lambda->formals exp)) (lambda->exp exp)) ())))
                                         (if (set!? exp)
                                            (__toplevel_cons
                                               'set!
                                               (__toplevel_cons
                                                  (substitute-var env (set!->var exp))
                                                  (__toplevel_cons (substitute env (set!->exp exp)) ())))
                                            (if (<change> (if? exp) (not (if? exp)))
                                               (__toplevel_cons
                                                  'if
                                                  (__toplevel_cons
                                                     (substitute env (if->condition exp))
                                                     (__toplevel_cons
                                                        (substitute env (if->then exp))
                                                        (__toplevel_cons (substitute env (if->else exp)) ()))))
                                               (if (let? exp)
                                                  (__toplevel_cons
                                                     'let
                                                     (__toplevel_cons
                                                        (azip (let->bound-vars exp) (map (substitute-with env) (let->args exp)))
                                                        (__toplevel_cons (substitute (assq-remove-keys env (let->bound-vars exp)) (let->exp exp)) ())))
                                                  (if (letrec? exp)
                                                     (let ((new-env (assq-remove-keys env (letrec->bound-vars exp))))
                                                        (__toplevel_cons
                                                           'letrec
                                                           (__toplevel_cons
                                                              (azip (letrec->bound-vars exp) (map (substitute-with new-env) (letrec->args exp)))
                                                              (__toplevel_cons (substitute new-env (letrec->exp exp)) ()))))
                                                     (if (begin? exp)
                                                        (cons 'begin (map (substitute-with env) (begin->exps exp)))
                                                        (if (cell? exp)
                                                           (__toplevel_cons 'cell (__toplevel_cons (substitute env (cell->value exp)) ()))
                                                           (if (<change> (cell-get? exp) (not (cell-get? exp)))
                                                              (__toplevel_cons 'cell-get (__toplevel_cons (substitute env (cell-get->cell exp)) ()))
                                                              (if (set-cell!? exp)
                                                                 (<change>
                                                                    (__toplevel_cons
                                                                       'set-cell!
                                                                       (__toplevel_cons
                                                                          (substitute env (set-cell!->cell exp))
                                                                          (__toplevel_cons (substitute env (set-cell!->value exp)) ())))
                                                                    (if (closure? exp)
                                                                       (__toplevel_cons
                                                                          'closure
                                                                          (__toplevel_cons
                                                                             (substitute env (closure->lam exp))
                                                                             (__toplevel_cons (substitute env (closure->env exp)) ())))
                                                                       (if (env-make? exp)
                                                                          (__toplevel_cons
                                                                             'env-make
                                                                             (__toplevel_cons
                                                                                (env-make->id exp)
                                                                                (__toplevel_append
                                                                                   (azip (env-make->fields exp) (map (substitute-with env) (env-make->values exp)))
                                                                                   ())))
                                                                          (if (env-get? exp)
                                                                             (if (app? exp)
                                                                                (map (substitute-with env) exp)
                                                                                (error "unhandled expression type in substitution: " exp))
                                                                             (__toplevel_cons
                                                                                'env-get
                                                                                (__toplevel_cons
                                                                                   (env-get->id exp)
                                                                                   (__toplevel_cons (env-get->field exp) (__toplevel_cons (substitute env (env-get->env exp)) ()))))))))
                                                                 (<change>
                                                                    (if (closure? exp)
                                                                       (__toplevel_cons
                                                                          'closure
                                                                          (__toplevel_cons
                                                                             (substitute env (closure->lam exp))
                                                                             (__toplevel_cons (substitute env (closure->env exp)) ())))
                                                                       (if (env-make? exp)
                                                                          (__toplevel_cons
                                                                             'env-make
                                                                             (__toplevel_cons
                                                                                (env-make->id exp)
                                                                                (__toplevel_append
                                                                                   (azip (env-make->fields exp) (map (substitute-with env) (env-make->values exp)))
                                                                                   ())))
                                                                          (if (env-get? exp)
                                                                             (__toplevel_cons
                                                                                'env-get
                                                                                (__toplevel_cons
                                                                                   (env-get->id exp)
                                                                                   (__toplevel_cons (env-get->field exp) (__toplevel_cons (substitute env (env-get->env exp)) ()))))
                                                                             (if (app? exp)
                                                                                (map (substitute-with env) exp)
                                                                                (error "unhandled expression type in substitution: " exp)))))
                                                                    (__toplevel_cons
                                                                       'set-cell!
                                                                       (__toplevel_cons
                                                                          (substitute env (set-cell!->cell exp))
                                                                          (__toplevel_cons (substitute env (set-cell!->value exp)) ()))))))))))))))))))))
         (let=>lambda (lambda (exp)
                        (if (let? exp)
                           (let ((vars (map car (let->bindings exp)))
                                 (args (map cadr (let->bindings exp))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'lambda
                                    (__toplevel_cons (__toplevel_append vars ()) (__toplevel_cons (let->exp exp) ())))
                                 (__toplevel_append args ())))
                           exp)))
         (letrec=>lets+sets (lambda (exp)
                              (if (letrec? exp)
                                 (let* ((bindings (letrec->bindings exp))
                                        (namings (map (lambda (b) (list (car b) #f)) bindings))
                                        (names (letrec->bound-vars exp))
                                        (sets (map (lambda (binding) (cons 'set! binding)) bindings))
                                        (args (letrec->args exp)))
                                    (__toplevel_cons
                                       'let
                                       (__toplevel_cons
                                          namings
                                          (__toplevel_cons
                                             (__toplevel_cons 'begin (__toplevel_append (append sets (list (letrec->exp exp))) ()))
                                             ()))))
                                 #f)))
         (begin=>let (lambda (exp)
                       (<change>
                          ()
                          exps)
                       (<change>
                          (letrec ((singlet? (lambda (l)
                                               (if (list? l) (= (length l) 1) #f)))
                                   (dummy-bind (lambda (exps)
                                                 (if (singlet? exps)
                                                    (car exps)
                                                    (if (pair? exps)
                                                       (__toplevel_cons
                                                          'let
                                                          (__toplevel_cons
                                                             (__toplevel_cons (__toplevel_cons '$_ (__toplevel_cons (car exps) ())) ())
                                                             (__toplevel_cons (dummy-bind (cdr exps)) ())))
                                                       #f)))))
                             (dummy-bind (begin->exps exp)))
                          ((lambda (x) x)
                             (letrec ((singlet? (lambda (l)
                                                  (if (list? l) (= (length l) 1) #f)))
                                      (dummy-bind (lambda (exps)
                                                    (if (singlet? exps)
                                                       (car exps)
                                                       (if (pair? exps)
                                                          (__toplevel_cons
                                                             'let
                                                             (__toplevel_cons
                                                                (__toplevel_cons (__toplevel_cons '$_ (__toplevel_cons (car exps) ())) ())
                                                                (__toplevel_cons (dummy-bind (cdr exps)) ())))
                                                          #f)))))
                                (dummy-bind (begin->exps exp)))))))
         (desugar (lambda (exp)
                    (if (const? exp)
                       exp
                       (if (<change> (prim? exp) (not (prim? exp)))
                          exp
                          (if (ref? exp)
                             exp
                             (if (lambda? exp)
                                (__toplevel_cons
                                   'lambda
                                   (__toplevel_cons (lambda->formals exp) (__toplevel_cons (desugar (lambda->exp exp)) ())))
                                (if (set!? exp)
                                   (__toplevel_cons 'set! (__toplevel_cons (set!->var exp) (__toplevel_cons (set!->exp exp) ())))
                                   (if (if? exp)
                                      (__toplevel_cons
                                         'if
                                         (__toplevel_cons
                                            (if->condition exp)
                                            (__toplevel_cons (if->then exp) (__toplevel_cons (if->else exp) ()))))
                                      (if (let? exp)
                                         (desugar (let=>lambda exp))
                                         (if (letrec? exp)
                                            (desugar (letrec=>lets+sets exp))
                                            (if (begin? exp)
                                               (desugar (begin=>let exp))
                                               (if (cell? exp)
                                                  (__toplevel_cons 'cell (__toplevel_cons (desugar (cell->value exp)) ()))
                                                  (if (cell-get? exp)
                                                     (__toplevel_cons 'cell-get (__toplevel_cons (desugar (cell-get->cell exp)) ()))
                                                     (if (<change> (set-cell!? exp) (not (set-cell!? exp)))
                                                        (__toplevel_cons
                                                           'set-cell!
                                                           (__toplevel_cons
                                                              (desugar (set-cell!->cell exp))
                                                              (__toplevel_cons (desugar (set-cell!->value exp)) ())))
                                                        (if (closure? exp)
                                                           (__toplevel_cons
                                                              'closure
                                                              (__toplevel_cons (desugar (closure->lam exp)) (__toplevel_cons (desugar (closure->env exp)) ())))
                                                           (if (env-make? exp)
                                                              (__toplevel_cons
                                                                 'env-make
                                                                 (__toplevel_cons
                                                                    (env-make->id exp)
                                                                    (__toplevel_append (azip (env-make->fields exp) (map desugar (env-make->values exp))) ())))
                                                              (if (env-get? exp)
                                                                 (<change>
                                                                    (__toplevel_cons
                                                                       'env-get
                                                                       (__toplevel_cons
                                                                          (env-get->id exp)
                                                                          (__toplevel_cons (env-get->field exp) (__toplevel_cons (env-get->env exp) ()))))
                                                                    (if (app? exp)
                                                                       (map desugar exp)
                                                                       (error "unknown exp: " exp)))
                                                                 (<change>
                                                                    (if (app? exp)
                                                                       (map desugar exp)
                                                                       (error "unknown exp: " exp))
                                                                    (__toplevel_cons
                                                                       'env-get
                                                                       (__toplevel_cons
                                                                          (env-get->id exp)
                                                                          (__toplevel_cons (env-get->field exp) (__toplevel_cons (env-get->env exp) ()))))))))))))))))))))))
         (free-vars (lambda (exp)
                      (if (const? exp)
                         ()
                         (if (prim? exp)
                            ()
                            (if (ref? exp)
                               (list exp)
                               (if (lambda? exp)
                                  (<change>
                                     (difference (free-vars (lambda->exp exp)) (lambda->formals exp))
                                     (if (if? exp)
                                        (union
                                           (free-vars (if->condition exp))
                                           (union (free-vars (if->then exp)) (free-vars (if->else exp))))
                                        (if (not (set!? exp))
                                           (union (list (set!->var exp)) (free-vars (set!->exp exp)))
                                           (if (let? exp)
                                              (free-vars (let=>lambda exp))
                                              (if (letrec? exp)
                                                 not-handled
                                                 (if (begin? exp)
                                                    (reduce union (map free-vars (begin->exps exp)) ())
                                                    (if (cell-get? exp)
                                                       (free-vars (cell-get->cell exp))
                                                       (if (cell? exp)
                                                          (free-vars (cell->value exp))
                                                          (if (set-cell!? exp)
                                                             (union (free-vars (set-cell!->cell exp)) (free-vars (set-cell!->value exp)))
                                                             (if (closure? exp)
                                                                (union (free-vars (closure->lam exp)) (free-vars (closure->env exp)))
                                                                (if (env-make? exp)
                                                                   (reduce union (map free-vars (env-make->values exp)) ())
                                                                   (if (env-get? exp)
                                                                      (free-vars (env-get->env exp))
                                                                      (if (app? exp)
                                                                         (reduce union (map free-vars exp) ())
                                                                         (error "unknown expression: " exp))))))))))))))
                                  (<change>
                                     (if (if? exp)
                                        (union
                                           (free-vars (if->condition exp))
                                           (union (free-vars (if->then exp)) (free-vars (if->else exp))))
                                        (if (set!? exp)
                                           (union (list (set!->var exp)) (free-vars (set!->exp exp)))
                                           (if (let? exp)
                                              (free-vars (let=>lambda exp))
                                              (if (letrec? exp)
                                                 not-handled
                                                 (if (begin? exp)
                                                    (reduce union (map free-vars (begin->exps exp)) ())
                                                    (if (cell-get? exp)
                                                       (free-vars (cell-get->cell exp))
                                                       (if (cell? exp)
                                                          (free-vars (cell->value exp))
                                                          (if (set-cell!? exp)
                                                             (union (free-vars (set-cell!->cell exp)) (free-vars (set-cell!->value exp)))
                                                             (if (closure? exp)
                                                                (union (free-vars (closure->lam exp)) (free-vars (closure->env exp)))
                                                                (if (env-make? exp)
                                                                   (reduce union (map free-vars (env-make->values exp)) ())
                                                                   (if (env-get? exp)
                                                                      (free-vars (env-get->env exp))
                                                                      (if (app? exp)
                                                                         (reduce union (map free-vars exp) ())
                                                                         (error "unknown expression: " exp)))))))))))))
                                     (difference (free-vars (lambda->exp exp)) (lambda->formals exp)))))))))
         (mutable-variables ())
         (mark-mutable (lambda (symbol)
                         (<change>
                            (set! mutable-variables (cons symbol mutable-variables))
                            ((lambda (x) x) (set! mutable-variables (cons symbol mutable-variables))))))
         (is-mutable? (lambda (symbol)
                        (letrec ((is-in? (lambda (S)
                                           (if (not (pair? S))
                                              #f
                                              (if (eq? (car S) symbol) #t (is-in? (cdr S)))))))
                           (is-in? mutable-variables))))
         (analyze-mutable-variables (lambda (exp)
                                      (if (const? exp)
                                         (void)
                                         (if (prim? exp)
                                            (<change>
                                               (void)
                                               (if (ref? exp)
                                                  (void)
                                                  (if (lambda? exp)
                                                     (if (set!? exp)
                                                        (begin
                                                           (mark-mutable (set!->var exp))
                                                           (analyze-mutable-variables (set!->exp exp)))
                                                        (if (if? exp)
                                                           (begin
                                                              (analyze-mutable-variables (if->condition exp))
                                                              ((lambda (x) x) (analyze-mutable-variables (if->then exp)))
                                                              (analyze-mutable-variables (if->else exp)))
                                                           (if (let? exp)
                                                              (begin
                                                                 (map analyze-mutable-variables (map cadr (let->bindings exp)))
                                                                 (analyze-mutable-variables (let->exp exp)))
                                                              (if (letrec? exp)
                                                                 (begin
                                                                    (map analyze-mutable-variables (map cadr (letrec->bindings exp)))
                                                                    (analyze-mutable-variables (letrec->exp exp)))
                                                                 (if (begin? exp)
                                                                    (begin
                                                                       (map analyze-mutable-variables (begin->exps exp))
                                                                       (void))
                                                                    (if (app? exp)
                                                                       (begin
                                                                          (map analyze-mutable-variables exp)
                                                                          (void))
                                                                       (error "unknown expression type: " exp)))))))
                                                     (analyze-mutable-variables (lambda->exp exp)))))
                                            (<change>
                                               (if (ref? exp)
                                                  (void)
                                                  (if (lambda? exp)
                                                     (analyze-mutable-variables (lambda->exp exp))
                                                     (if (set!? exp)
                                                        (begin
                                                           (mark-mutable (set!->var exp))
                                                           (analyze-mutable-variables (set!->exp exp)))
                                                        (if (if? exp)
                                                           (begin
                                                              (analyze-mutable-variables (if->condition exp))
                                                              (analyze-mutable-variables (if->then exp))
                                                              (analyze-mutable-variables (if->else exp)))
                                                           (if (let? exp)
                                                              (begin
                                                                 (map analyze-mutable-variables (map cadr (let->bindings exp)))
                                                                 (analyze-mutable-variables (let->exp exp)))
                                                              (if (letrec? exp)
                                                                 (begin
                                                                    (map analyze-mutable-variables (map cadr (letrec->bindings exp)))
                                                                    (analyze-mutable-variables (letrec->exp exp)))
                                                                 (if (begin? exp)
                                                                    (begin
                                                                       (map analyze-mutable-variables (begin->exps exp))
                                                                       (void))
                                                                    (if (app? exp)
                                                                       (begin
                                                                          (map analyze-mutable-variables exp)
                                                                          (void))
                                                                       (error "unknown expression type: " exp)))))))))
                                               (void))))))
         (wrap-mutables (lambda (exp)
                          (letrec ((wrap-mutable-formals (lambda (formals body-exp)
                                                           (if (not (pair? formals))
                                                              body-exp
                                                              (if (is-mutable? (car formals))
                                                                 (__toplevel_cons
                                                                    'let
                                                                    (__toplevel_cons
                                                                       (__toplevel_cons
                                                                          (__toplevel_cons
                                                                             (car formals)
                                                                             (__toplevel_cons (__toplevel_cons 'cell (__toplevel_cons (car formals) ())) ()))
                                                                          ())
                                                                       (__toplevel_cons (wrap-mutable-formals (cdr formals) body-exp) ())))
                                                                 (wrap-mutable-formals (cdr formals) body-exp))))))
                             (if (const? exp)
                                exp
                                (if (ref? exp)
                                   (if (is-mutable? exp)
                                      (__toplevel_cons 'cell-get (__toplevel_cons exp ()))
                                      exp)
                                   (if (prim? exp)
                                      exp
                                      (if (lambda? exp)
                                         (__toplevel_cons
                                            'lambda
                                            (__toplevel_cons
                                               (lambda->formals exp)
                                               (__toplevel_cons (wrap-mutable-formals (lambda->formals exp) (wrap-mutables (lambda->exp exp))) ())))
                                         (if (set!? exp)
                                            (__toplevel_cons
                                               'set-cell!
                                               (__toplevel_cons (set!->var exp) (__toplevel_cons (wrap-mutables (set!->exp exp)) ())))
                                            (if (if? exp)
                                               (__toplevel_cons
                                                  'if
                                                  (__toplevel_cons
                                                     (wrap-mutables (if->condition exp))
                                                     (__toplevel_cons
                                                        (wrap-mutables (if->then exp))
                                                        (__toplevel_cons (wrap-mutables (if->else exp)) ()))))
                                               (if (app? exp)
                                                  (map wrap-mutables exp)
                                                  (error "unknown expression type: " exp)))))))))))
         (mangle (lambda (symbol)
                   (letrec ((m (lambda (chars)
                                 (if (null? chars)
                                    ()
                                    (if (let ((__or_res (if (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)) #f))) (if __or_res __or_res (char-numeric? (car chars))))
                                       (cons (car chars) (m (cdr chars)))
                                       (cons #\_ (append (integer->char-list (char->natural (car chars))) (m (cdr chars)))))))))
                      (list->string (m (string->list (symbol->string symbol)))))))
         (num-environments 0)
         (environments ())
         (allocate-environment (lambda (fields)
                                 (<change>
                                    (let ((id num-environments))
                                       (set! num-environments (+ 1 num-environments))
                                       (set! environments (cons (cons id fields) environments))
                                       id)
                                    ((lambda (x) x)
                                       (let ((id num-environments))
                                          (set! num-environments (+ 1 num-environments))
                                          (set! environments (cons (cons id fields) environments))
                                          (<change>
                                             ()
                                             id)
                                          id)))))
         (get-environment (lambda (id)
                            (cdr (assv id environments))))
         (closure-convert (lambda (exp)
                            (if (const? exp)
                               exp
                               (if (prim? exp)
                                  exp
                                  (if (ref? exp)
                                     exp
                                     (if (lambda? exp)
                                        (let* (($env (gensym 'env))
                                               (body (closure-convert (lambda->exp exp)))
                                               (fv (difference (free-vars body) (lambda->formals exp)))
                                               (id (allocate-environment fv))
                                               (sub (map
                                                      (lambda (v)
                                                         (list
                                                            v
                                                            (__toplevel_cons 'env-get (__toplevel_cons id (__toplevel_cons v (__toplevel_cons $env ()))))))
                                                      fv)))
                                           (<change>
                                              (__toplevel_cons
                                                 'closure
                                                 (__toplevel_cons
                                                    (__toplevel_cons
                                                       'lambda
                                                       (__toplevel_cons
                                                          (__toplevel_cons $env (__toplevel_append (lambda->formals exp) ()))
                                                          (__toplevel_cons (substitute sub body) ())))
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'env-make (__toplevel_cons id (__toplevel_append (azip fv fv) ())))
                                                       ())))
                                              ((lambda (x) x)
                                                 (__toplevel_cons
                                                    'closure
                                                    (__toplevel_cons
                                                       (__toplevel_cons
                                                          'lambda
                                                          (__toplevel_cons
                                                             (__toplevel_cons $env (__toplevel_append (lambda->formals exp) ()))
                                                             (__toplevel_cons (substitute sub body) ())))
                                                       (__toplevel_cons
                                                          (__toplevel_cons 'env-make (__toplevel_cons id (__toplevel_append (azip fv fv) ())))
                                                          ()))))))
                                        (if (if? exp)
                                           (__toplevel_cons
                                              'if
                                              (__toplevel_cons
                                                 (closure-convert (if->condition exp))
                                                 (__toplevel_cons
                                                    (closure-convert (if->then exp))
                                                    (__toplevel_cons (closure-convert (if->else exp)) ()))))
                                           (if (<change> (set!? exp) (not (set!? exp)))
                                              (__toplevel_cons
                                                 'set!
                                                 (__toplevel_cons (set!->var exp) (__toplevel_cons (closure-convert (set!->exp exp)) ())))
                                              (if (cell? exp)
                                                 (__toplevel_cons 'cell (__toplevel_cons (closure-convert (cell->value exp)) ()))
                                                 (if (cell-get? exp)
                                                    (__toplevel_cons 'cell-get (__toplevel_cons (closure-convert (cell-get->cell exp)) ()))
                                                    (if (set-cell!? exp)
                                                       (__toplevel_cons
                                                          'set-cell!
                                                          (__toplevel_cons
                                                             (closure-convert (set-cell!->cell exp))
                                                             (__toplevel_cons (closure-convert (set-cell!->value exp)) ())))
                                                       (if (app? exp)
                                                          (<change>
                                                             (map closure-convert exp)
                                                             (error "unhandled exp: " exp))
                                                          (<change>
                                                             (error "unhandled exp: " exp)
                                                             (map closure-convert exp))))))))))))))
         (c-compile-program (lambda (exp)
                              (let* ((preamble "")
                                     (append-preamble (lambda (s)
                                                        (set! preamble (string-append preamble "  " s "\n"))))
                                     (body (c-compile-exp exp append-preamble)))
                                 (string-append
                                    "int main (int argc, char* argv[]) {\n"
                                    preamble
                                    "  __sum         = MakePrimitive(__prim_sum) ;\n"
                                    "  __product     = MakePrimitive(__prim_product) ;\n"
                                    "  __difference  = MakePrimitive(__prim_difference) ;\n"
                                    "  __display     = MakePrimitive(__prim_display) ;\n"
                                    "  __numEqual    = MakePrimitive(__prim_numEqual) ;\n"
                                    "  "
                                    body
                                    " ;\n"
                                    "  return 0;\n"
                                    " }\n"))))
         (c-compile-exp (lambda (exp append-preamble)
                          (if (const? exp)
                             (c-compile-const exp)
                             (if (prim? exp)
                                (c-compile-prim exp)
                                (if (ref? exp)
                                   (c-compile-ref exp)
                                   (if (if? exp)
                                      (c-compile-if exp append-preamble)
                                      (if (cell? exp)
                                         (c-compile-cell exp append-preamble)
                                         (if (cell-get? exp)
                                            (c-compile-cell-get exp append-preamble)
                                            (if (set-cell!? exp)
                                               (c-compile-set-cell! exp append-preamble)
                                               (if (closure? exp)
                                                  (c-compile-closure exp append-preamble)
                                                  (if (env-make? exp)
                                                     (c-compile-env-make exp append-preamble)
                                                     (if (env-get? exp)
                                                        (c-compile-env-get exp append-preamble)
                                                        (if (app? exp)
                                                           (c-compile-app exp append-preamble)
                                                           (error "unknown exp in c-compile-exp: " exp))))))))))))))
         (c-compile-const (lambda (exp)
                            (if (integer? exp)
                               (string-append "MakeInt(" (number->string exp) ")")
                               (if (boolean? exp)
                                  (string-append "MakeBoolean(" (if exp "1" "0") ")")
                                  (error "unknown constant: " exp)))))
         (c-compile-prim (lambda (p)
                           (if (eq? '+ p)
                              "__sum"
                              (if (eq? '- p)
                                 "__difference"
                                 (if (eq? '* p)
                                    "__product"
                                    (if (eq? '= p)
                                       "__numEqual"
                                       (if (<change> (eq? 'display p) (not (eq? 'display p)))
                                          "__display"
                                          (error "unhandled primitive: " p))))))))
         (c-compile-ref (lambda (exp)
                          (mangle exp)))
         (c-compile-args (lambda (args append-preamble)
                           (<change>
                              (if (not (pair? args))
                                 ""
                                 (string-append
                                    (c-compile-exp (car args) append-preamble)
                                    (if (pair? (cdr args))
                                       (string-append ", " (c-compile-args (cdr args) append-preamble))
                                       "")))
                              ((lambda (x) x)
                                 (if (not (pair? args))
                                    ""
                                    (string-append
                                       (c-compile-exp (car args) append-preamble)
                                       (if (pair? (cdr args))
                                          (string-append ", " (c-compile-args (cdr args) append-preamble))
                                          "")))))))
         (c-compile-app (lambda (exp append-preamble)
                          (let (($tmp (mangle (gensym 'tmp))))
                             (<change>
                                (append-preamble (string-append "Value " $tmp " ; "))
                                ())
                             (let* ((args (app->args exp))
                                    (fun (app->fun exp)))
                                (string-append
                                   "("
                                   $tmp
                                   " = "
                                   (c-compile-exp fun append-preamble)
                                   ","
                                   $tmp
                                   ".clo.lam("
                                   "MakeEnv("
                                   $tmp
                                   ".clo.env)"
                                   (if (null? args) "" ",")
                                   (c-compile-args args append-preamble)
                                   "))")))))
         (c-compile-if (lambda (exp append-preamble)
                         (string-append
                            "("
                            (c-compile-exp (if->condition exp) append-preamble)
                            ").b.value ? "
                            "("
                            (c-compile-exp (if->then exp) append-preamble)
                            ") : "
                            "("
                            (c-compile-exp (if->else exp) append-preamble)
                            ")")))
         (c-compile-set-cell! (lambda (exp append-preamble)
                                (string-append
                                   "(*"
                                   "("
                                   (c-compile-exp (set-cell!->cell exp) append-preamble)
                                   ".cell.addr)"
                                   " = "
                                   (c-compile-exp (set-cell!->value exp) append-preamble)
                                   ")")))
         (c-compile-cell-get (lambda (exp append-preamble)
                               (string-append "(*(" (c-compile-exp (cell-get->cell exp) append-preamble) ".cell.addr" "))")))
         (c-compile-cell (lambda (exp append-preamble)
                           (string-append "NewCell(" (c-compile-exp (cell->value exp) append-preamble) ")")))
         (c-compile-env-make (lambda (exp append-preamble)
                               (string-append
                                  "MakeEnv(__alloc_env"
                                  (number->string (env-make->id exp))
                                  "("
                                  (c-compile-args (env-make->values exp) append-preamble)
                                  "))")))
         (c-compile-env-get (lambda (exp append-preamble)
                              (string-append
                                 "((struct __env_"
                                 (number->string (env-get->id exp))
                                 "*)"
                                 (c-compile-exp (env-get->env exp) append-preamble)
                                 ".env.env)->"
                                 (mangle (env-get->field exp)))))
         (num-lambdas 0)
         (lambdas ())
         (allocate-lambda (lambda (lam)
                            (let ((id num-lambdas))
                               (set! num-lambdas (+ 1 num-lambdas))
                               (set! lambdas (cons (list id lam) lambdas))
                               id)))
         (get-lambda (lambda (id)
                       (cdr (assv id lambdas))))
         (c-compile-closure (lambda (exp append-preamble)
                              (let* ((lam (closure->lam exp))
                                     (env (closure->env exp))
                                     (lid (allocate-lambda (c-compile-lambda lam))))
                                 (string-append
                                    "MakeClosure("
                                    "__lambda_"
                                    (number->string lid)
                                    ","
                                    (c-compile-exp env append-preamble)
                                    ")"))))
         (c-compile-formals (lambda (formals)
                              (if (not (pair? formals))
                                 ""
                                 (string-append
                                    "Value "
                                    (mangle (car formals))
                                    (if (pair? (cdr formals))
                                       (string-append ", " (c-compile-formals (cdr formals)))
                                       "")))))
         (c-compile-lambda (lambda (exp)
                             (let* ((preamble "")
                                    (append-preamble (lambda (s)
                                                       (set! preamble (string-append preamble "  " s "\n")))))
                                (let ((formals (c-compile-formals (lambda->formals exp)))
                                      (body (c-compile-exp (lambda->exp exp) append-preamble)))
                                   (lambda (name)
                                      (string-append "Value " name "(" formals ") {\n" preamble "  return " body " ;\n" "}\n"))))))
         (c-compile-env-struct (lambda (env)
                                 (let* ((id (car env))
                                        (fields (cdr env))
                                        (sid (number->string id))
                                        (tyname (string-append "struct __env_" sid)))
                                    (string-append
                                       "struct __env_"
                                       (number->string id)
                                       " {\n"
                                       (apply string-append (map (lambda (f) (string-append " Value " (mangle f) " ; \n")) fields))
                                       "} ;\n\n"
                                       tyname
                                       "*"
                                       " __alloc_env"
                                       sid
                                       "("
                                       (c-compile-formals fields)
                                       ")"
                                       "{\n"
                                       "  "
                                       tyname
                                       "*"
                                       " t = malloc(sizeof("
                                       tyname
                                       "))"
                                       ";\n"
                                       (apply
                                          string-append
                                          (map (lambda (f) (string-append "  t->" (mangle f) " = " (mangle f) ";\n")) fields))
                                       "  return t;\n"
                                       "}\n\n"))))
         (emit (lambda (line)
                 (display line)
                 (newline)))
         (c-compile-and-emit (lambda (emit input-program)
                               (letrec ((compiled-program ""))
                                  (set! input-program (desugar input-program))
                                  (<change>
                                     (analyze-mutable-variables input-program)
                                     (set! input-program (desugar (wrap-mutables input-program))))
                                  (<change>
                                     (set! input-program (desugar (wrap-mutables input-program)))
                                     (analyze-mutable-variables input-program))
                                  (set! input-program (closure-convert input-program))
                                  (<change>
                                     (emit "#include <stdlib.h>")
                                     ())
                                  (emit "#include <stdio.h>")
                                  (emit "#include \"scheme.h\"")
                                  (<change>
                                     ()
                                     (emit "#include \"scheme.h\""))
                                  (<change>
                                     (emit "")
                                     ((lambda (x) x) (emit "")))
                                  (emit
                                     "
Value __sum ;
Value __difference ;
Value __product ;
Value __display ;
Value __numEqual ;
")
                                  (<change>
                                     (for-each (lambda (env) (emit (c-compile-env-struct env))) environments)
                                     (set! compiled-program (c-compile-program input-program)))
                                  (<change>
                                     (set! compiled-program (c-compile-program input-program))
                                     (for-each (lambda (env) (emit (c-compile-env-struct env))) environments))
                                  (emit "Value __prim_sum(Value e, Value a, Value b) {
  return MakeInt(a.z.value + b.z.value) ;
}")
                                  (emit
                                     "Value __prim_product(Value e, Value a, Value b) {
  return MakeInt(a.z.value * b.z.value) ;
}")
                                  (emit
                                     "Value __prim_difference(Value e, Value a, Value b) {
  return MakeInt(a.z.value - b.z.value) ;
}")
                                  (<change>
                                     (emit "Value __prim_display(Value e, Value v) {
  printf(\"%i\\n\",v.z.value) ;
  return v ;
}")
                                     ())
                                  (emit
                                     "Value __prim_numEqual(Value e, Value a, Value b) {
  return MakeBoolean(a.z.value == b.z.value) ;
}")
                                  (for-each
                                     (lambda (l)
                                        (emit (string-append "Value __lambda_" (number->string (car l)) "() ;")))
                                     lambdas)
                                  (emit "")
                                  (for-each
                                     (lambda (l)
                                        (emit ((cadr l) (string-append "__lambda_" (number->string (car l))))))
                                     lambdas)
                                  (emit compiled-program))))
         (the-program 3))
   (c-compile-and-emit emit the-program))