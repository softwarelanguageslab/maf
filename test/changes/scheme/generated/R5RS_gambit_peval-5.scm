; Changes:
; * removed: 3
; * added: 4
; * swaps: 0
; * negated predicates: 3
; * swapped branches: 4
; * calls to id fun: 2
(letrec ((every? (lambda (pred? l)
                   (let ((__or_res (null? l)))
                      (if __or_res
                         __or_res
                         (if (pred? (car l)) (every? pred? (cdr l)) #f)))))
         (some? (lambda (pred? l)
                  (if (null? l)
                     #f
                     (let ((__or_res (pred? (car l))))
                        (if __or_res __or_res (some? pred? (cdr l)))))))
         (map2 (lambda (f l1 l2)
                 (if (pair? l1)
                    (cons (f (car l1) (car l2)) (map2 f (cdr l1) (cdr l2)))
                    ())))
         (get-last-pair (lambda (l)
                          (let ((x (cdr l)))
                             (if (pair? x) (get-last-pair x) l))))
         (partial-evaluate (lambda (proc args)
                             (peval (alphatize proc ()) args)))
         (alphatize (lambda (exp env)
                      (letrec ((alpha (lambda (exp)
                                        (if (const-expr? exp)
                                           (quot (const-value exp))
                                           (if (symbol? exp)
                                              (let ((x (assq exp env)))
                                                 (if x (cdr x) exp))
                                              (if (let ((__or_res (eq? (car exp) 'if))) (if __or_res __or_res (eq? (car exp) 'begin)))
                                                 (cons (car exp) (map alpha (cdr exp)))
                                                 (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                                                    (let ((new-env (new-variables (map car (cadr exp)) env)))
                                                       (list
                                                          (car exp)
                                                          (map
                                                             (lambda (x)
                                                                (list
                                                                   (cdr (assq (car x) new-env))
                                                                   (if (eq? (car exp) 'let)
                                                                      (alpha (cadr x))
                                                                      (alphatize (cadr x) new-env))))
                                                             (cadr exp))
                                                          (alphatize (caddr exp) new-env)))
                                                    (if (eq? (car exp) 'lambda)
                                                       (let ((new-env (new-variables (cadr exp) env)))
                                                          (list 'lambda (map (lambda (x) (cdr (assq x new-env))) (cadr exp)) (alphatize (caddr exp) new-env)))
                                                       (map alpha exp)))))))))
                         (alpha exp))))
         (const-expr? (lambda (expr)
                        (if (not (symbol? expr))
                           (let ((__or_res (not (pair? expr))))
                              (if __or_res __or_res (eq? (car expr) 'quote)))
                           #f)))
         (const-value (lambda (expr)
                        (if (pair? expr) (cadr expr) expr)))
         (quot (lambda (val)
                 (list 'quote val)))
         (new-variables (lambda (parms env)
                          (append (map (lambda (x) (cons x (new-variable x))) parms) env)))
         (*current-num* 0)
         (new-variable (lambda (name)
                         (<change>
                            (set! *current-num* (+ *current-num* 1))
                            ())
                         (string->symbol (string-append (symbol->string name) "_" (number->string *current-num*)))))
         (peval (lambda (proc args)
                  (simplify!
                     (let ((parms (cadr proc))
                           (body (caddr proc)))
                        (list
                           'lambda
                           (remove-constant parms args)
                           (beta-subst
                              body
                              (map2 (lambda (x y) (if (not-constant? y) (__toplevel_cons () ()) (cons x (quot y)))) parms args)))))))
         (not-constant (list '?))
         (not-constant? (lambda (x)
                          (eq? x not-constant)))
         (remove-constant (lambda (l a)
                            (if (null? l)
                               ()
                               (if (not-constant? (car a))
                                  (cons (car l) (remove-constant (cdr l) (cdr a)))
                                  (remove-constant (cdr l) (cdr a))))))
         (extract-constant (lambda (l a)
                             (if (null? l)
                                ()
                                (if (not-constant? (car a))
                                   (extract-constant (cdr l) (cdr a))
                                   (cons (car l) (extract-constant (cdr l) (cdr a)))))))
         (beta-subst (lambda (exp env)
                       (<change>
                          ()
                          eq?)
                       (letrec ((bs (lambda (exp)
                                      (if (<change> (const-expr? exp) (not (const-expr? exp)))
                                         (quot (const-value exp))
                                         (if (symbol? exp)
                                            (let ((x (assq exp env)))
                                               (if x (cdr x) exp))
                                            (if (let ((__or_res (eq? (car exp) 'if))) (<change> () exp) (if __or_res __or_res (eq? (car exp) 'begin)))
                                               (cons (car exp) (map bs (cdr exp)))
                                               (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                                                  (list (car exp) (map (lambda (x) (list (car x) (bs (cadr x)))) (cadr exp)) (bs (caddr exp)))
                                                  (if (eq? (car exp) 'lambda)
                                                     (list 'lambda (cadr exp) (bs (caddr exp)))
                                                     (map bs exp)))))))))
                          (bs exp))))
         (simplify! (lambda (exp)
                      (letrec ((simp! (lambda (where env)
                                        (letrec ((s! (lambda (where)
                                                       (let ((exp (car where)))
                                                          (let ((__cond-empty-body (const-expr? exp)))
                                                             (if __cond-empty-body
                                                                __cond-empty-body
                                                                (let ((__cond-empty-body (symbol? exp)))
                                                                   (if __cond-empty-body
                                                                      __cond-empty-body
                                                                      (if (eq? (car exp) 'if)
                                                                         (begin
                                                                            (s! (cdr exp))
                                                                            (if (const-expr? (cadr exp))
                                                                               (begin
                                                                                  (set-car!
                                                                                     where
                                                                                     (if (memq (const-value (cadr exp)) (__toplevel_cons #f (__toplevel_cons () ())))
                                                                                        (if (= (length exp) 3)
                                                                                           (__toplevel_cons 'quote (__toplevel_cons () ()))
                                                                                           (cadddr exp))
                                                                                        (caddr exp)))
                                                                                  (s! where))
                                                                               (for-each! s! (cddr exp))))
                                                                         (if (eq? (car exp) 'begin)
                                                                            (begin
                                                                               (for-each! s! (cdr exp))
                                                                               ((letrec ((loop (lambda (exps)
                                                                                                (if (not (null? (cddr exps)))
                                                                                                   (let ((x (cadr exps)))
                                                                                                      (loop
                                                                                                         (if (let ((__or_res (const-expr? x))) (if __or_res __or_res (let ((__or_res (symbol? x))) (if __or_res __or_res (if (pair? x) (eq? (car x) 'lambda) #f)))))
                                                                                                            (begin
                                                                                                               (set-cdr! exps (cddr exps))
                                                                                                               exps)
                                                                                                            (cdr exps))))
                                                                                                   #f))))
                                                                                  loop)
                                                                                  exp)
                                                                               (if (null? (cddr exp))
                                                                                  (set-car! where (cadr exp))
                                                                                  #f))
                                                                            (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                                                                               (let ((new-env (cons exp env)))
                                                                                  (letrec ((keep (lambda (i)
                                                                                                   (if (>= i (length (cadar where)))
                                                                                                      ()
                                                                                                      (let* ((var (car (list-ref (cadar where) i)))
                                                                                                             (val (cadr (assq var (cadar where))))
                                                                                                             (refs (ref-count (car where) var))
                                                                                                             (self-refs (ref-count val var))
                                                                                                             (total-refs (- (car refs) (car self-refs)))
                                                                                                             (oper-refs (- (cadr refs) (cadr self-refs))))
                                                                                                         (if (= total-refs 0)
                                                                                                            (keep (+ i 1))
                                                                                                            (if (let ((__or_res (const-expr? val))) (if __or_res __or_res (let ((__or_res (symbol? val))) (if __or_res __or_res (let ((__or_res (if (pair? val) (if (eq? (car val) 'lambda) (if (= total-refs 1) (if (= oper-refs 1) (= (car self-refs) 0) #f) #f) #f) #f))) (if __or_res __or_res (if (caddr refs) (= total-refs 1) #f)))))))
                                                                                                               (begin
                                                                                                                  (set-car! where (beta-subst (car where) (list (cons var val))))
                                                                                                                  (keep (+ i 1)))
                                                                                                               (cons var (keep (+ i 1))))))))))
                                                                                     (simp! (cddr exp) new-env)
                                                                                     (for-each! (lambda (x) (simp! (cdar x) new-env)) (cadr exp))
                                                                                     (let ((to-keep (keep 0)))
                                                                                        (if (< (length to-keep) (length (cadar where)))
                                                                                           (begin
                                                                                              (if (null? to-keep)
                                                                                                 (set-car! where (caddar where))
                                                                                                 (set-car! (cdar where) (map (lambda (v) (assq v (cadar where))) to-keep)))
                                                                                              (s! where))
                                                                                           (if (null? to-keep)
                                                                                              (set-car! where (caddar where))
                                                                                              #f)))))
                                                                               (if (eq? (car exp) 'lambda)
                                                                                  (simp! (cddr exp) (cons exp env))
                                                                                  (begin
                                                                                     (for-each! s! exp)
                                                                                     (if (symbol? (car exp))
                                                                                        (let ((frame (binding-frame (car exp) env)))
                                                                                           (if frame
                                                                                              (let ((proc (bound-expr (car exp) frame)))
                                                                                                 (if (if (pair? proc) (if (eq? (car proc) 'lambda) (some? const-expr? (cdr exp)) #f) #f)
                                                                                                    (let* ((args (arg-pattern (cdr exp)))
                                                                                                           (new-proc (peval proc args))
                                                                                                           (new-args (remove-constant (cdr exp) args)))
                                                                                                       (set-car! where (cons (add-binding new-proc frame (car exp)) new-args)))
                                                                                                    #f))
                                                                                              (set-car! where (constant-fold-global (car exp) (cdr exp)))))
                                                                                        (let ((__cond-empty-body (not (pair? (car exp)))))
                                                                                           (if __cond-empty-body
                                                                                              __cond-empty-body
                                                                                              (if (eq? (caar exp) 'lambda)
                                                                                                 (begin
                                                                                                    (set-car! where (list 'let (map2 list (cadar exp) (cdr exp)) (caddar exp)))
                                                                                                    (s! where))
                                                                                                 #f)))))))))))))))))
                                           (s! where))))
                               (remove-empty-calls! (lambda (where env)
                                                      (letrec ((rec! (lambda (where)
                                                                       (let ((exp (car where)))
                                                                          (let ((__cond-empty-body (const-expr? exp)))
                                                                             (if __cond-empty-body
                                                                                __cond-empty-body
                                                                                (let ((__cond-empty-body (symbol? exp)))
                                                                                   (if __cond-empty-body
                                                                                      __cond-empty-body
                                                                                      (if (eq? (car exp) 'if)
                                                                                         (begin
                                                                                            (rec! (cdr exp))
                                                                                            (rec! (cddr exp))
                                                                                            (rec! (cdddr exp)))
                                                                                         (if (eq? (car exp) 'begin)
                                                                                            (for-each! rec! (cdr exp))
                                                                                            (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                                                                                               (let ((new-env (cons exp env)))
                                                                                                  (remove-empty-calls! (cddr exp) new-env)
                                                                                                  (for-each! (lambda (x) (remove-empty-calls! (cdar x) new-env)) (cadr exp)))
                                                                                               (if (eq? (car exp) 'lambda)
                                                                                                  (rec! (cddr exp))
                                                                                                  (begin
                                                                                                     (for-each! rec! (cdr exp))
                                                                                                     (if (if (null? (cdr exp)) (symbol? (car exp)) #f)
                                                                                                        (let ((frame (binding-frame (car exp) env)))
                                                                                                           (if frame
                                                                                                              (let ((proc (bound-expr (car exp) frame)))
                                                                                                                 (if (if (pair? proc) (eq? (car proc) 'lambda) #f)
                                                                                                                    (begin
                                                                                                                       (set! changed? #t)
                                                                                                                       (set-car! where (caddr proc)))
                                                                                                                    #f))
                                                                                                              #f))
                                                                                                        #f))))))))))))))
                                                         (rec! where))))
                               (changed? #f))
                         (let ((x (list exp)))
                            ((letrec ((loop (lambda ()
                                             (set! changed? #f)
                                             (simp! x ())
                                             (remove-empty-calls! x ())
                                             (if changed? (loop) (car x)))))
                               loop))))))
         (ref-count (lambda (exp var)
                      (let ((total 0)
                            (oper 0)
                            (always-evaled #t))
                         (letrec ((rc (lambda (exp ae)
                                        (let ((__cond-empty-body (const-expr? exp)))
                                           (if __cond-empty-body
                                              __cond-empty-body
                                              (if (symbol? exp)
                                                 (if (eq? exp var)
                                                    (begin
                                                       (set! total (+ total 1))
                                                       (set! always-evaled (if ae
                                                                           (<change>
                                                                              always-evaled
                                                                              #f)
                                                                           (<change>
                                                                              #f
                                                                              always-evaled))))
                                                    #f)
                                                 (if (eq? (car exp) 'if)
                                                    (begin
                                                       (rc (cadr exp) ae)
                                                       (for-each (lambda (x) (rc x #f)) (cddr exp)))
                                                    (if (eq? (car exp) 'begin)
                                                       (for-each (lambda (x) (rc x ae)) (cdr exp))
                                                       (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                                                          (begin
                                                             (<change>
                                                                (for-each (lambda (x) (rc (cadr x) ae)) (cadr exp))
                                                                ())
                                                             (rc (caddr exp) ae))
                                                          (if (eq? (car exp) 'lambda)
                                                             (rc (caddr exp) #f)
                                                             (begin
                                                                (for-each (lambda (x) (rc x ae)) exp)
                                                                (if (symbol? (car exp))
                                                                   (if (eq? (car exp) var)
                                                                      (set! oper (+ oper 1))
                                                                      #f)
                                                                   #f))))))))))))
                            (rc exp #t)
                            (list total oper always-evaled)))))
         (binding-frame (lambda (var env)
                          (if (null? env)
                             #f
                             (if (let ((__or_res (eq? (caar env) 'let))) (if __or_res __or_res (eq? (caar env) 'letrec)))
                                (if (assq var (cadar env))
                                   (car env)
                                   (binding-frame var (cdr env)))
                                (if (eq? (caar env) 'lambda)
                                   (if (memq var (cadar env))
                                      (<change>
                                         (car env)
                                         (binding-frame var (cdr env)))
                                      (<change>
                                         (binding-frame var (cdr env))
                                         (car env)))
                                   (error "ill-formed environment"))))))
         (bound-expr (lambda (var frame)
                       (if (let ((__or_res (eq? (car frame) 'let))) (if __or_res (<change> __or_res (eq? (car frame) 'letrec)) (<change> (eq? (car frame) 'letrec) __or_res)))
                          (cadr (assq var (cadr frame)))
                          (if (eq? (car frame) 'lambda)
                             not-constant
                             (error "ill-formed frame")))))
         (add-binding (lambda (val frame name)
                        (letrec ((find-val (lambda (val bindings)
                                             (if (null? bindings)
                                                #f
                                                (if (equal? val (cadar bindings))
                                                   (caar bindings)
                                                   (find-val val (cdr bindings)))))))
                           (<change>
                              ()
                              (display var))
                           (let ((__or_res (find-val val (cadr frame))))
                              (if __or_res
                                 __or_res
                                 (let ((var (new-variable name)))
                                    (<change>
                                       (set-cdr! (get-last-pair (cadr frame)) (list (list var val)))
                                       ())
                                    var))))))
         (for-each! (lambda (proc! l)
                      (if (not (null? l))
                         (begin
                            (proc! l)
                            (for-each! proc! (cdr l)))
                         #f)))
         (arg-pattern (lambda (exps)
                        (if (null? exps)
                           ()
                           (cons (if (const-expr? (car exps)) (const-value (car exps)) not-constant) (arg-pattern (cdr exps))))))
         (*primitives* (list
                         (cons
                            'car
                            (lambda (args)
                               (<change>
                                  ()
                                  (display #f))
                               (if (= (length args) 1)
                                  (if (pair? (car args))
                                     (quot (car (car args)))
                                     #f)
                                  #f)))
                         (cons
                            'cdr
                            (lambda (args)
                               (if (= (length args) 1)
                                  (if (pair? (car args))
                                     (quot (cdr (car args)))
                                     #f)
                                  #f)))
                         (cons '+ (lambda (args) (if (every? number? args) (quot (sum args 0)) #f)))
                         (cons
                            '*
                            (lambda (args)
                               (<change>
                                  (if (every? number? args)
                                     (quot (product args 1))
                                     #f)
                                  ((lambda (x) x) (if (every? number? args) (quot (product args 1)) #f)))))
                         (cons
                            '-
                            (lambda (args)
                               (if (> (length args) 0)
                                  (if (every? number? args)
                                     (quot (if (null? (cdr args)) (- (car args)) (- (car args) (sum (cdr args) 0))))
                                     #f)
                                  #f)))
                         (cons
                            '/
                            (lambda (args)
                               (if (> (length args) 1)
                                  (if (every? number? args)
                                     (quot
                                        (if (<change> (null? (cdr args)) (not (null? (cdr args))))
                                           (/ (car args))
                                           (/ (car args) (product (cdr args) 1))))
                                     #f)
                                  #f)))
                         (cons
                            '<
                            (lambda (args)
                               (if (= (length args) 2)
                                  (if (every? number? args)
                                     (quot (< (car args) (cadr args)))
                                     #f)
                                  #f)))
                         (cons
                            '=
                            (lambda (args)
                               (<change>
                                  (if (= (length args) 2)
                                     (if (every? number? args)
                                        (quot (= (car args) (cadr args)))
                                        #f)
                                     #f)
                                  ((lambda (x) x)
                                     (if (= (length args) 2)
                                        (if (every? number? args)
                                           (quot (= (car args) (cadr args)))
                                           #f)
                                        #f)))))
                         (cons
                            '>
                            (lambda (args)
                               (if (<change> (= (length args) 2) (not (= (length args) 2)))
                                  (if (every? number? args)
                                     (quot (> (car args) (cadr args)))
                                     #f)
                                  #f)))
                         (cons 'eq? (lambda (args) (if (= (length args) 2) (quot (eq? (car args) (cadr args))) #f)))
                         (cons 'not (lambda (args) (if (= (length args) 1) (quot (not (car args))) #f)))
                         (cons 'null? (lambda (args) (if (= (length args) 1) (quot (null? (car args))) #f)))
                         (cons 'pair? (lambda (args) (if (= (length args) 1) (quot (pair? (car args))) #f)))
                         (cons 'symbol? (lambda (args) (if (= (length args) 1) (quot (symbol? (car args))) #f)))))
         (sum (lambda (lst n)
                (if (null? lst)
                   n
                   (sum (cdr lst) (+ n (car lst))))))
         (product (lambda (lst n)
                    (if (null? lst)
                       n
                       (product (cdr lst) (* n (car lst))))))
         (reduce-global (lambda (name args)
                          (let ((x (assq name *primitives*)))
                             (if x ((cdr x) args) #f))))
         (constant-fold-global (lambda (name exprs)
                                 (letrec ((flatten (lambda (args op)
                                                     (if (null? args)
                                                        ()
                                                        (if (if (pair? (car args)) (<change> (eq? (caar args) op) #f) (<change> #f (eq? (caar args) op)))
                                                           (append (flatten (cdar args) op) (flatten (cdr args) op))
                                                           (cons (car args) (flatten (cdr args) op)))))))
                                    (let ((args (if (let ((__or_res (eq? name '+))) (if __or_res __or_res (eq? name '*)))
                                                  (flatten exprs name)
                                                  exprs)))
                                       (let ((__or_res (if (every? const-expr? args)
                                                         (reduce-global name (map const-value args))
                                                         #f)))
                                          (if __or_res
                                             __or_res
                                             (let ((pattern (arg-pattern args)))
                                                (let ((non-const (remove-constant args pattern))
                                                      (const (map const-value (extract-constant args pattern))))
                                                   (if (eq? name '+)
                                                      (let ((x (reduce-global '+ const)))
                                                         (if x
                                                            (let ((y (const-value x)))
                                                               (cons '+ (if (= y 0) non-const (cons x non-const))))
                                                            (cons name args)))
                                                      (if (eq? name '*)
                                                         (let ((x (reduce-global '* const)))
                                                            (if x
                                                               (let ((y (const-value x)))
                                                                  (cons '* (if (= y 1) non-const (cons x non-const))))
                                                               (cons name args)))
                                                         (if (eq? name 'cons)
                                                            (if (if (const-expr? (cadr args)) (null? (const-value (cadr args))) #f)
                                                               (list 'list (car args))
                                                               (if (if (pair? (cadr args)) (eq? (car (cadr args)) 'list) #f)
                                                                  (cons 'list (cons (car args) (cdr (cadr args))))
                                                                  (cons name args)))
                                                            (cons name args))))))))))))
         (try-peval (lambda (proc args)
                      (partial-evaluate proc args)))
         (example1 (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
                        (__toplevel_cons
                           (__toplevel_cons
                              'if
                              (__toplevel_cons
                                 (__toplevel_cons 'null? (__toplevel_cons 'a ()))
                                 (__toplevel_cons
                                    'b
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          '+
                                          (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'a ())) (__toplevel_cons 'c ())))
                                       ()))))
                           ()))))
         (example2 (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'x (__toplevel_cons 'y ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              'let
                              (__toplevel_cons
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'q
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'lambda
                                             (__toplevel_cons
                                                (__toplevel_cons 'a (__toplevel_cons 'b ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'if
                                                      (__toplevel_cons
                                                         (__toplevel_cons '< (__toplevel_cons 'a (__toplevel_cons 0 ())))
                                                         (__toplevel_cons
                                                            'b
                                                            (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 10 (__toplevel_cons 'b ()))) ()))))
                                                   ())))
                                          ()))
                                    ())
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'if
                                       (__toplevel_cons
                                          (__toplevel_cons '< (__toplevel_cons 'x (__toplevel_cons 0 ())))
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'q
                                                (__toplevel_cons
                                                   (__toplevel_cons '- (__toplevel_cons 'y ()))
                                                   (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'x ())) ())))
                                             (__toplevel_cons (__toplevel_cons 'q (__toplevel_cons 'y (__toplevel_cons 'x ()))) ()))))
                                    ())))
                           ()))))
         (example3 (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'l (__toplevel_cons 'n ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              'letrec
                              (__toplevel_cons
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'add-list
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'lambda
                                             (__toplevel_cons
                                                (__toplevel_cons 'l (__toplevel_cons 'n ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'if
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'null? (__toplevel_cons 'l ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'quote (__toplevel_cons () ()))
                                                            (__toplevel_cons
                                                               (__toplevel_cons
                                                                  'cons
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        '+
                                                                        (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'l ())) (__toplevel_cons 'n ())))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons
                                                                           'add-list
                                                                           (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'l ())) (__toplevel_cons 'n ())))
                                                                        ())))
                                                               ()))))
                                                   ())))
                                          ()))
                                    ())
                                 (__toplevel_cons (__toplevel_cons 'add-list (__toplevel_cons 'l (__toplevel_cons 'n ()))) ())))
                           ()))))
         (example4 (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'exp (__toplevel_cons 'env ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              'letrec
                              (__toplevel_cons
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'eval
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'lambda
                                             (__toplevel_cons
                                                (__toplevel_cons 'exp (__toplevel_cons 'env ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'letrec
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            (__toplevel_cons
                                                               'eval-list
                                                               (__toplevel_cons
                                                                  (__toplevel_cons
                                                                     'lambda
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'l (__toplevel_cons 'env ()))
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons
                                                                              'if
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'null? (__toplevel_cons 'l ()))
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons 'quote (__toplevel_cons () ()))
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons
                                                                                          'cons
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons
                                                                                                'eval
                                                                                                (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'l ())) (__toplevel_cons 'env ())))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons
                                                                                                   'eval-list
                                                                                                   (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'l ())) (__toplevel_cons 'env ())))
                                                                                                ())))
                                                                                       ()))))
                                                                           ())))
                                                                  ()))
                                                            ())
                                                         (__toplevel_cons
                                                            (__toplevel_cons
                                                               'if
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'symbol? (__toplevel_cons 'exp ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons 'lookup (__toplevel_cons 'exp (__toplevel_cons 'env ())))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons
                                                                           'if
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'pair? (__toplevel_cons 'exp ())) ()))
                                                                              (__toplevel_cons
                                                                                 'exp
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons
                                                                                       'if
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons
                                                                                             'eq?
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons 'car (__toplevel_cons 'exp ()))
                                                                                                (__toplevel_cons (__toplevel_cons 'quote (__toplevel_cons 'quote ())) ())))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons 'car (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'exp ())) ()))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons
                                                                                                   'apply
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons
                                                                                                         'eval
                                                                                                         (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'exp ())) (__toplevel_cons 'env ())))
                                                                                                      (__toplevel_cons
                                                                                                         (__toplevel_cons
                                                                                                            'eval-list
                                                                                                            (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'exp ())) (__toplevel_cons 'env ())))
                                                                                                         ())))
                                                                                                ()))))
                                                                                    ()))))
                                                                        ()))))
                                                            ())))
                                                   ())))
                                          ()))
                                    ())
                                 (__toplevel_cons (__toplevel_cons 'eval (__toplevel_cons 'exp (__toplevel_cons 'env ()))) ())))
                           ()))))
         (example5 (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'a (__toplevel_cons 'b ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              'letrec
                              (__toplevel_cons
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'funct
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'lambda
                                             (__toplevel_cons
                                                (__toplevel_cons 'x ())
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      '+
                                                      (__toplevel_cons
                                                         'x
                                                         (__toplevel_cons
                                                            'b
                                                            (__toplevel_cons
                                                               (__toplevel_cons
                                                                  'if
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons '< (__toplevel_cons 'x (__toplevel_cons 1 ())))
                                                                     (__toplevel_cons
                                                                        0
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons
                                                                              'funct
                                                                              (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'x (__toplevel_cons 1 ()))) ()))
                                                                           ()))))
                                                               ()))))
                                                   ())))
                                          ()))
                                    ())
                                 (__toplevel_cons (__toplevel_cons 'funct (__toplevel_cons 'a ())) ())))
                           ()))))
         (example6 (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        ()
                        (__toplevel_cons
                           (__toplevel_cons
                              'letrec
                              (__toplevel_cons
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'fib
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'lambda
                                             (__toplevel_cons
                                                (__toplevel_cons 'x ())
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'if
                                                      (__toplevel_cons
                                                         (__toplevel_cons '< (__toplevel_cons 'x (__toplevel_cons 2 ())))
                                                         (__toplevel_cons
                                                            'x
                                                            (__toplevel_cons
                                                               (__toplevel_cons
                                                                  '+
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'fib
                                                                        (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'x (__toplevel_cons 1 ()))) ()))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons
                                                                           'fib
                                                                           (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'x (__toplevel_cons 2 ()))) ()))
                                                                        ())))
                                                               ()))))
                                                   ())))
                                          ()))
                                    ())
                                 (__toplevel_cons (__toplevel_cons 'fib (__toplevel_cons 10 ())) ())))
                           ()))))
         (example7 (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'input ())
                        (__toplevel_cons
                           (__toplevel_cons
                              'letrec
                              (__toplevel_cons
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'copy
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'lambda
                                             (__toplevel_cons
                                                (__toplevel_cons 'in ())
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'if
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'pair? (__toplevel_cons 'in ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons
                                                               'cons
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'copy (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'in ())) ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons 'copy (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'in ())) ()))
                                                                     ())))
                                                            (__toplevel_cons 'in ()))))
                                                   ())))
                                          ()))
                                    ())
                                 (__toplevel_cons (__toplevel_cons 'copy (__toplevel_cons 'input ())) ())))
                           ()))))
         (example8 (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'input ())
                        (__toplevel_cons
                           (__toplevel_cons
                              'letrec
                              (__toplevel_cons
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'reverse
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'lambda
                                             (__toplevel_cons
                                                (__toplevel_cons 'in (__toplevel_cons 'result ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'if
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'pair? (__toplevel_cons 'in ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons
                                                               'reverse
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'cdr (__toplevel_cons 'in ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'cons
                                                                        (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'in ())) (__toplevel_cons 'result ())))
                                                                     ())))
                                                            (__toplevel_cons 'result ()))))
                                                   ())))
                                          ()))
                                    ())
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'reverse
                                       (__toplevel_cons 'input (__toplevel_cons (__toplevel_cons 'quote (__toplevel_cons () ())) ())))
                                    ())))
                           ()))))
         (test (lambda ()
                 (set! *current-num* 0)
                 (list
                    (try-peval example1 (list (__toplevel_cons 10 (__toplevel_cons 11 ())) not-constant 1))
                    (try-peval example2 (list not-constant 1))
                    (try-peval example3 (list not-constant 1))
                    (try-peval
                       example3
                       (list (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) not-constant))
                    (try-peval example4 (list 'x not-constant))
                    (try-peval
                       example4
                       (list
                          (__toplevel_cons 'f (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
                          not-constant))
                    (try-peval example5 (list 5 not-constant))
                    (try-peval example6 ())
                    (try-peval
                       example7
                       (list
                          (__toplevel_cons
                             'a
                             (__toplevel_cons
                                'b
                                (__toplevel_cons
                                   'c
                                   (__toplevel_cons
                                      'd
                                      (__toplevel_cons
                                         'e
                                         (__toplevel_cons
                                            'f
                                            (__toplevel_cons
                                               'g
                                               (__toplevel_cons
                                                  'h
                                                  (__toplevel_cons
                                                     'i
                                                     (__toplevel_cons
                                                        'j
                                                        (__toplevel_cons
                                                           'k
                                                           (__toplevel_cons
                                                              'l
                                                              (__toplevel_cons
                                                                 'm
                                                                 (__toplevel_cons
                                                                    'n
                                                                    (__toplevel_cons
                                                                       'o
                                                                       (__toplevel_cons
                                                                          'p
                                                                          (__toplevel_cons
                                                                             'q
                                                                             (__toplevel_cons
                                                                                'r
                                                                                (__toplevel_cons
                                                                                   's
                                                                                   (__toplevel_cons
                                                                                      't
                                                                                      (__toplevel_cons
                                                                                         'u
                                                                                         (__toplevel_cons
                                                                                            'v
                                                                                            (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))))))))))))))))))))))))
                    (try-peval
                       example8
                       (list
                          (__toplevel_cons
                             'a
                             (__toplevel_cons
                                'b
                                (__toplevel_cons
                                   'c
                                   (__toplevel_cons
                                      'd
                                      (__toplevel_cons
                                         'e
                                         (__toplevel_cons
                                            'f
                                            (__toplevel_cons
                                               'g
                                               (__toplevel_cons
                                                  'h
                                                  (__toplevel_cons
                                                     'i
                                                     (__toplevel_cons
                                                        'j
                                                        (__toplevel_cons
                                                           'k
                                                           (__toplevel_cons
                                                              'l
                                                              (__toplevel_cons
                                                                 'm
                                                                 (__toplevel_cons
                                                                    'n
                                                                    (__toplevel_cons
                                                                       'o
                                                                       (__toplevel_cons
                                                                          'p
                                                                          (__toplevel_cons
                                                                             'q
                                                                             (__toplevel_cons
                                                                                'r
                                                                                (__toplevel_cons
                                                                                   's
                                                                                   (__toplevel_cons
                                                                                      't
                                                                                      (__toplevel_cons
                                                                                         'u
                                                                                         (__toplevel_cons
                                                                                            'v
                                                                                            (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))))))))))))))))))))))))))))
   (let ((result (test)))
      (if (list? result)
         (if (= (length result) 10)
            (equal?
               (list-ref result 9)
               (__toplevel_cons
                  'lambda
                  (__toplevel_cons
                     ()
                     (__toplevel_cons
                        (__toplevel_cons
                           'list
                           (__toplevel_cons
                              (__toplevel_cons 'quote (__toplevel_cons 'z ()))
                              (__toplevel_cons
                                 (__toplevel_cons 'quote (__toplevel_cons 'y ()))
                                 (__toplevel_cons
                                    (__toplevel_cons 'quote (__toplevel_cons 'x ()))
                                    (__toplevel_cons
                                       (__toplevel_cons 'quote (__toplevel_cons 'w ()))
                                       (__toplevel_cons
                                          (__toplevel_cons 'quote (__toplevel_cons 'v ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'quote (__toplevel_cons 'u ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'quote (__toplevel_cons 't ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons 'quote (__toplevel_cons 's ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'quote (__toplevel_cons 'r ()))
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'quote (__toplevel_cons 'q ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'quote (__toplevel_cons 'p ()))
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'quote (__toplevel_cons 'o ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'quote (__toplevel_cons 'n ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons 'quote (__toplevel_cons 'm ()))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'quote (__toplevel_cons 'l ()))
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons 'quote (__toplevel_cons 'k ()))
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'quote (__toplevel_cons 'j ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'quote (__toplevel_cons 'i ()))
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons 'quote (__toplevel_cons 'h ()))
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons 'quote (__toplevel_cons 'g ()))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons 'quote (__toplevel_cons 'f ()))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons 'quote (__toplevel_cons 'e ()))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons 'quote (__toplevel_cons 'd ()))
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons 'quote (__toplevel_cons 'c ()))
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons 'quote (__toplevel_cons 'b ()))
                                                                                                      (__toplevel_cons (__toplevel_cons 'quote (__toplevel_cons 'a ())) ())))))))))))))))))))))))))))
                        ()))))
            #f)
         #f)))