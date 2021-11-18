; Changes:
; * removed: 0
; * added: 8
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 3
; * calls to id fun: 2
(letrec ((string->list (lambda (s)
                         (letrec ((f (lambda (i)
                                       (if (< i (string-length s))
                                          (cons (string-ref s i) (f (+ i 1)))
                                          ()))))
                            (f 0))))
         (void (lambda ()
                 (if #f #t #f)))
         (tagged-list? (lambda (tag l)
                         (if (pair? l) (eq? tag (car l)) #f)))
         (char->natural (lambda (c)
                          (let ((i (char->integer c)))
                             (if (< i 0) (* -2 i) (+ (* 2 i) 1)))))
         (integer->char-list (lambda (n)
                               (string->list (number->string n))))
         (const? (lambda (exp)
                   (integer? exp)))
         (ref? (lambda (exp)
                 (symbol? exp)))
         (let? (lambda (exp)
                 (<change>
                    ()
                    'let)
                 (tagged-list? 'let exp)))
         (let->bindings (lambda (exp)
                          (cadr exp)))
         (let->exp (lambda (exp)
                     (caddr exp)))
         (letrec1? (lambda (exp)
                     (if (tagged-list? 'letrec exp)
                        (= (length (cadr exp)) 1)
                        #f)))
         (letrec1->binding (lambda (exp)
                             (caadr exp)))
         (letrec1->exp (lambda (exp)
                         (caddr exp)))
         (lambda? (lambda (exp)
                    (tagged-list? 'lambda exp)))
         (lambda->formals (lambda (exp)
                            (<change>
                               (cadr exp)
                               ((lambda (x) x) (cadr exp)))))
         (lambda->exp (lambda (exp)
                        (caddr exp)))
         (if? (lambda (exp)
                (tagged-list? 'if exp)))
         (if->condition (lambda (exp)
                          (cadr exp)))
         (if->then (lambda (exp)
                     (<change>
                        ()
                        (display caddr))
                     (caddr exp)))
         (if->else (lambda (exp)
                     (cadddr exp)))
         (app? (lambda (exp)
                 (pair? exp)))
         (app->fun (lambda (exp)
                     (car exp)))
         (app->args (lambda (exp)
                      (cdr exp)))
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
                   (<change>
                      ()
                      exp)
                   (tagged-list? 'begin exp)))
         (begin2exps (lambda (exp)
                       (cdr exp)))
         (isset!? (lambda (exp)
                    (tagged-list? 'set! exp)))
         (set-var (lambda (exp)
                    (<change>
                       (cadr exp)
                       ((lambda (x) x) (cadr exp)))))
         (set-exp (lambda (exp)
                    (caddr exp)))
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
         (arity (lambda (lam)
                  (length (lambda->formals lam))))
         (xargs (lambda (n)
                  (if (<= n 0)
                     ()
                     (cons (string->symbol (string-append "x" (number->string n))) (xargs (- n 1))))))
         (Yn (lambda (n)
               (__toplevel_cons
                  (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'h ())
                        (__toplevel_cons
                           (__toplevel_cons
                              'lambda
                              (__toplevel_cons
                                 (__toplevel_cons 'F ())
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'F
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'lambda
                                             (__toplevel_cons
                                                (__toplevel_append (xargs n) ())
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      (__toplevel_cons (__toplevel_cons 'h (__toplevel_cons 'h ())) (__toplevel_cons 'F ()))
                                                      (__toplevel_append (xargs n) ()))
                                                   ())))
                                          ()))
                                    ())))
                           ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'lambda
                        (__toplevel_cons
                           (__toplevel_cons 'h ())
                           (__toplevel_cons
                              (__toplevel_cons
                                 'lambda
                                 (__toplevel_cons
                                    (__toplevel_cons 'F ())
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'F
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'lambda
                                                (__toplevel_cons
                                                   (__toplevel_append (xargs n) ())
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         (__toplevel_cons (__toplevel_cons 'h (__toplevel_cons 'h ())) (__toplevel_cons 'F ()))
                                                         (__toplevel_append (xargs n) ()))
                                                      ())))
                                             ()))
                                       ())))
                              ())))
                     ()))))
         (letrec1=>Y (lambda (exp)
                       (if (letrec1? exp)
                          (<change>
                             (let* ((binding (letrec1->binding exp))
                                    (name (car binding))
                                    (arg (cadr binding))
                                    (num-args (arity arg)))
                                (__toplevel_cons
                                   'let
                                   (__toplevel_cons
                                      (__toplevel_cons
                                         (__toplevel_cons
                                            name
                                            (__toplevel_cons
                                               (__toplevel_cons
                                                  (Yn num-args)
                                                  (__toplevel_cons
                                                     (__toplevel_cons 'lambda (__toplevel_cons (__toplevel_cons name ()) (__toplevel_cons arg ())))
                                                     ()))
                                               ()))
                                         ())
                                      (__toplevel_cons (letrec1->exp exp) ()))))
                             exp)
                          (<change>
                             exp
                             (let* ((binding (letrec1->binding exp))
                                    (name (car binding))
                                    (arg (cadr binding))
                                    (num-args (arity arg)))
                                (__toplevel_cons
                                   'let
                                   (__toplevel_cons
                                      (__toplevel_cons
                                         (__toplevel_cons
                                            name
                                            (__toplevel_cons
                                               (__toplevel_cons
                                                  (Yn num-args)
                                                  (__toplevel_cons
                                                     (__toplevel_cons 'lambda (__toplevel_cons (__toplevel_cons name ()) (__toplevel_cons arg ())))
                                                     ()))
                                               ()))
                                         ())
                                      (__toplevel_cons (letrec1->exp exp) ()))))))))
         (begin=>let (lambda (exp)
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
                          (dummy-bind (begin2exps exp)))))
         (mutable-variables ())
         (mark-mutable (lambda (symbol)
                         (set! mutable-variables (cons symbol mutable-variables))))
         (is-mutable? (lambda (symbol)
                        (letrec ((is-in? (lambda (S)
                                           (if (not (pair? S))
                                              #f
                                              (if (eq? (car S) symbol) #t (is-in? (cdr S)))))))
                           (is-in? mutable-variables))))
         (analyze-mutable-variables (lambda (exp)
                                      (if (const? exp)
                                         (<change>
                                            (void)
                                            (if (ref? exp)
                                               (void)
                                               (if (not (prim? exp))
                                                  (void)
                                                  (if (lambda? exp)
                                                     (analyze-mutable-variables (lambda->exp exp))
                                                     (if (let? exp)
                                                        (begin
                                                           (map analyze-mutable-variables (map cadr (let->bindings exp)))
                                                           (analyze-mutable-variables (let->exp exp)))
                                                        (if (letrec1? exp)
                                                           (begin
                                                              (analyze-mutable-variables (cadr (letrec1->binding exp)))
                                                              (display (analyze-mutable-variables (letrec1->exp exp)))
                                                              (analyze-mutable-variables (letrec1->exp exp)))
                                                           (if (isset!? exp)
                                                              (mark-mutable (set-var exp))
                                                              (if (if? exp)
                                                                 (begin
                                                                    (analyze-mutable-variables (if->then exp))
                                                                    (analyze-mutable-variables (if->condition exp))
                                                                    (analyze-mutable-variables (if->else exp)))
                                                                 (if (begin? exp)
                                                                    (begin
                                                                       (map analyze-mutable-variables (begin2exps exp))
                                                                       (void))
                                                                    (if (app? exp)
                                                                       (begin
                                                                          (map analyze-mutable-variables exp)
                                                                          (void))
                                                                       (error "unknown expression type: " exp)))))))))))
                                         (<change>
                                            (if (ref? exp)
                                               (void)
                                               (if (prim? exp)
                                                  (void)
                                                  (if (lambda? exp)
                                                     (analyze-mutable-variables (lambda->exp exp))
                                                     (if (let? exp)
                                                        (begin
                                                           (map analyze-mutable-variables (map cadr (let->bindings exp)))
                                                           (analyze-mutable-variables (let->exp exp)))
                                                        (if (letrec1? exp)
                                                           (begin
                                                              (analyze-mutable-variables (cadr (letrec1->binding exp)))
                                                              (analyze-mutable-variables (letrec1->exp exp)))
                                                           (if (isset!? exp)
                                                              (mark-mutable (set-var exp))
                                                              (if (if? exp)
                                                                 (begin
                                                                    (analyze-mutable-variables (if->condition exp))
                                                                    (analyze-mutable-variables (if->then exp))
                                                                    (analyze-mutable-variables (if->else exp)))
                                                                 (if (begin? exp)
                                                                    (begin
                                                                       (map analyze-mutable-variables (begin2exps exp))
                                                                       (void))
                                                                    (if (app? exp)
                                                                       (begin
                                                                          (map analyze-mutable-variables exp)
                                                                          (void))
                                                                       (error "unknown expression type: " exp))))))))))
                                            (void)))))
         (mangle (lambda (symbol)
                   (<change>
                      ()
                      (display #\_))
                   (<change>
                      ()
                      (display symbol->string))
                   (letrec ((m (lambda (chars)
                                 (if (null? chars)
                                    ()
                                    (if (let ((__or_res (if (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)) #f))) (if __or_res __or_res (char-numeric? (car chars))))
                                       (cons (car chars) (m (cdr chars)))
                                       (cons #\_ (append (integer->char-list (char->natural (car chars))) (m (cdr chars)))))))))
                      (list->string (m (string->list (symbol->string symbol)))))))
         (java-compile-program (lambda (exp)
                                 (string-append
                                    "public class BOut extends RuntimeEnvironment {\n"
                                    " public static void main (String[] args) {\n"
                                    (java-compile-exp exp)
                                    " ;\n"
                                    " }\n"
                                    "}\n")))
         (java-compile-exp (lambda (exp)
                             (if (const? exp)
                                (java-compile-const exp)
                                (if (prim? exp)
                                   (java-compile-prim exp)
                                   (if (ref? exp)
                                      (java-compile-ref exp)
                                      (if (lambda? exp)
                                         (java-compile-lambda exp)
                                         (if (if? exp)
                                            (java-compile-if exp)
                                            (if (isset!? exp)
                                               (java-compile-set! exp)
                                               (if (let? exp)
                                                  (java-compile-exp (let=>lambda exp))
                                                  (if (letrec1? exp)
                                                     (java-compile-exp (letrec1=>Y exp))
                                                     (if (begin? exp)
                                                        (java-compile-exp (begin=>let exp))
                                                        (if (app? exp) (java-compile-app exp) #f))))))))))))
         (java-compile-const (lambda (exp)
                               (<change>
                                  ()
                                  (string-append "new IntValue(" (number->string exp) ")"))
                               (if (integer? exp)
                                  (string-append "new IntValue(" (number->string exp) ")")
                                  (error "unknown constant: " exp))))
         (java-compile-prim (lambda (p)
                              (if (eq? '+ p)
                                 "sum"
                                 (if (eq? '- p)
                                    "difference"
                                    (if (eq? '* p)
                                       "product"
                                       (if (eq? '= p)
                                          "numEqual"
                                          (if (eq? 'display p)
                                             "display"
                                             (error "unhandled primitive " p))))))))
         (java-compile-ref (lambda (exp)
                             (if (is-mutable? exp)
                                (string-append "m_" (mangle exp) ".value")
                                (mangle exp))))
         (java-compile-formals (lambda (formals)
                                 (if (not (pair? formals))
                                    ""
                                    (string-append
                                       "final Value "
                                       (mangle (car formals))
                                       (if (pair? (cdr formals))
                                          (<change>
                                             (string-append ", " (java-compile-formals (cdr formals)))
                                             "")
                                          (<change>
                                             ""
                                             (string-append ", " (java-compile-formals (cdr formals)))))))))
         (java-compile-lambda (lambda (exp)
                                (letrec ((java-wrap-mutables (lambda (vars)
                                                               (if (not (pair? vars))
                                                                  ""
                                                                  (string-append
                                                                     (if (is-mutable? (car vars))
                                                                        (string-append
                                                                           " final ValueCell m_"
                                                                           (mangle (car vars))
                                                                           " = new ValueCell("
                                                                           (mangle (car vars))
                                                                           ");\n")
                                                                        "")
                                                                     (java-wrap-mutables (cdr vars)))))))
                                   (let* ((formals (lambda->formals exp))
                                          (num-args (length formals)))
                                      (string-append
                                         "new NullProcValue"
                                         (number->string num-args)
                                         " () {\n"
                                         " public Value apply("
                                         (java-compile-formals formals)
                                         ") {\n"
                                         (java-wrap-mutables formals)
                                         "\n"
                                         "  return "
                                         (java-compile-exp (lambda->exp exp))
                                         " ;\n"
                                         "}}\n")))))
         (java-compile-args (lambda (args)
                              (if (not (pair? args))
                                 ""
                                 (string-append
                                    (java-compile-exp (car args))
                                    (if (pair? (cdr args))
                                       (string-append ", " (java-compile-args (cdr args)))
                                       "")))))
         (java-compile-set! (lambda (exp)
                              (<change>
                                 ()
                                 "VoidValue.Void(m_")
                              (string-append
                                 "VoidValue.Void(m_"
                                 (mangle (set-var exp))
                                 ".value = "
                                 (java-compile-exp (set-exp exp))
                                 ")")))
         (java-compile-app (lambda (exp)
                             (let* ((args (app->args exp))
                                    (fun (app->fun exp))
                                    (num-args (length args)))
                                (string-append
                                   "((ProcValue"
                                   (number->string num-args)
                                   ")("
                                   (java-compile-exp fun)
                                   ")).apply("
                                   (java-compile-args args)
                                   ")\n"))))
         (java-compile-if (lambda (exp)
                            (string-append
                               "("
                               (java-compile-exp (if->condition exp))
                               ").toBoolean() ? ("
                               (java-compile-exp (if->then exp))
                               ") : ("
                               (java-compile-exp (if->else exp))
                               ")")))
         (input-program 3))
   (analyze-mutable-variables input-program)
   (display (java-compile-program input-program)))