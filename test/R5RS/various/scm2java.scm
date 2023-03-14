;; Expected result: "public class BOut extends RuntimeEnvironment {\n public static void main (String[] args) {\nnew IntValue(3) ;\n }\n}\n"
(letrec ((map (lambda (f lst)
                (if (pair? lst)
                    (cons (f (car lst))
                          (map f (cdr lst)))
                    '())))
         (append (lambda (lst1 lst2)
                   (if (not (pair? lst1))
                       lst2
                       (cons (car lst1)
                             (append (cdr lst1) lst2)))))
         (string->list (lambda (s)
                         (letrec ((f (lambda (i)
                                       (if (< i (string-length s))
                                           (cons (string-ref s i)
                                                 (f (+ i 1)))
                                           '()))))
                           (f 0))))
         (tagged-list? (lambda (tag l)
                         (and (pair? l) (eq? tag (car l)))))
         (char->natural (lambda (c)
                          (let ((i (char->integer c)))
                            (if (< i 0)
                                (* -2 i)
                                (+ (* 2 i) 1)))))
         (integer->char-list (lambda (n)
                               (string->list (number->string n))))
         (const? (lambda (exp) (integer? exp)))
         (ref? (lambda (exp) (symbol? exp)))
         (let? (lambda (exp) (tagged-list? 'let exp)))
         (let->bindings (lambda (exp) (cadr exp)))
         (let->exp (lambda (exp) (caddr exp)))
         (letrec1? (lambda (exp)
                     (and (tagged-list? 'letrec exp)
                          (= (length (cadr exp)) 1))))
         (letrec1->binding (lambda (exp) (caadr exp)))
         (letrec1->exp (lambda (exp) (caddr exp)))
         (lambda? (lambda (exp) (tagged-list? 'lambda exp)))
         (lambda->formals (lambda (exp) (cadr exp)))
         (lambda->exp (lambda (exp) (caddr exp)))
         (if? (lambda (exp) (tagged-list? 'if exp)))
         (if->condition (lambda (exp) (cadr exp)))
         (if->then (lambda (exp) (caddr exp)))
         (if->else (lambda (exp) (cadddr exp)))
         (app? (lambda (exp) (pair? exp)))
         (app->fun (lambda (exp) (car exp)))
         (app->args (lambda (exp) (cdr exp)))
         (prim? (lambda (exp)
                  (or (eq? exp '+)
                      (eq? exp '-)
                      (eq? exp '*)
                      (eq? exp '=)
                      (eq? exp 'display))))
         (begin? (lambda (exp) (tagged-list? 'begin exp)))
         (begin->exps (lambda (exp) (cdr exp)))
         (set!? (lambda (exp) (tagged-list? 'set! exp)))
         (set!-var (lambda (exp) (cadr exp)))
         (set!-exp (lambda (exp) (caddr exp)))
         (let=>lambda (lambda (exp)
                        (if (let? exp)
                            (let ((vars (map car (let->bindings exp)))
                                  (args (map cadr (let->bindings exp))))
                              (cons (cons 'lambda (cons vars (cons (let->exp exp) '()))) args))
                            exp)))
         (arity (lambda (lam)
                  (length (lambda->formals lam))))
         (xargs (lambda (n)
                  (if (<= n 0)
                      '()
                      (cons (string->symbol (string-append "x" (number->string n)))
                            (xargs (- n 1))))))
         (Yn (lambda (n)
               (cons
                (cons 'lambda
                      (cons (cons 'h '())
                            (cons
                             (cons 'lambda
                                   (cons (cons 'F '())
                                         (cons (cons 'F
                                                     (cons (cons 'lambda
                                                                 (cons (xargs n)
                                                                       (cons
                                                                        (cons
                                                                         (cons (cons 'h (cons 'h '())) (cons 'F '()))
                                                                         (xargs n))
                                                                        '())))
                                                           '()))
                                               '())))
                             '())))
                (cons (cons 'lambda
                            (cons (cons 'h '())
                                  (cons
                                   (cons 'lambda
                                         (cons (cons 'F '())
                                               (cons (cons 'F
                                                           (cons (cons 'lambda
                                                                       (cons (xargs n)
                                                                             (cons
                                                                              (cons
                                                                               (cons (cons 'h (cons 'h '())) (cons 'F '()))
                                                                               (xargs n))
                                                                              '())))
                                                                 '()))
                                                     '())))
                                   '())))
                      '()))))
         (letrec1=>Y (lambda (exp)
                       (if (letrec1? exp)
                           (let* ((binding  (letrec1->binding exp))
                                  (name     (car binding))
                                  (arg      (cadr binding))
                                  (num-args (arity arg)))
                             (cons 'let
                                   (cons
                                    (cons (cons name
                                                (cons (cons (Yn num-args)
                                                            (cons (cons 'lambda (cons (cons name '()) (cons arg '())))
                                                                  '()))
                                                      '()))
                                          '())
                                    (cons (letrec1->exp exp) '()))))
                           exp)))
         (singlet? (lambda (l)
                        (and (list? l)
                             (= (length l) 1))))
         (dummy-bind (lambda (exps)
                       (if (singlet? exps)
                           (car exps)
                           (if (pair? exps)
                               (cons 'let
                                     (cons
                                      (cons (cons '$_
                                                  (cons (car exps) '()))
                                            '())
                                      (cons (dummy-bind (cdr exps)) '())))
                               (error "no match")))))
         (begin=>let (lambda (exp)
                       (dummy-bind (begin->exps exp))))
         (mutable-variables '())
         (mark-mutable (lambda (symbol)
                         (set! mutable-variables (cons symbol mutable-variables))))
         (is-in? (lambda (S symbol)
                   (if (not (pair? S))
                       #f
                       (if (eq? (car S) symbol)
                           #t
                           (is-in? (cdr S) symbol)))))
         (is-mutable? (lambda (symbol)
                        (is-in? mutable-variables symbol)))
         (analyze-mutable-variables (lambda (exp)
                                      (if (const? exp) #f
                                          (if (ref? exp) #f
                                              (if (prim? exp) #f
                                                  (if (lambda? exp) (analyze-mutable-variables (lambda->exp exp))
                                                      (if (let? exp) (begin
                                                                       (map analyze-mutable-variables (map cadr (let->bindings exp)))
                                                                       (analyze-mutable-variables (let->exp exp)))
                                                          (if (letrec1? exp) (begin (analyze-mutable-variables (cadr (letrec1->binding exp)))
                                                                                    (analyze-mutable-variables (letrec1->exp exp)))
                                                              (if (set!? exp) (mark-mutable (set!-var exp))
                                                                  (if (if? exp) (begin (analyze-mutable-variables (if->condition exp))
                                                                                       (analyze-mutable-variables (if->then exp))
                                                                                       (analyze-mutable-variables (if->else exp)))
                                                                      (if (begin? exp) (begin (map analyze-mutable-variables (begin->exps exp))
                                                                                              #f)
                                                                          (if (app? exp) (begin (map analyze-mutable-variables exp)
                                                                                                #f)
                                                                              (error "unknown expression type: " exp)))))))))))))
         (m (lambda (chars)
                  (if (null? chars)
                      '()
                      (cons (car chars) (m (cdr chars))))))
         (mangle (lambda (symbol)
                   (list->string (m (string->list (symbol->string symbol))))))
         (java-compile-const (lambda (exp)
                               (if (integer? exp)
                                   (string-append "new IntValue(" (number->string exp) ")")
                                   (error "unknown constant: " exp))))
         (java-compile-prim (lambda (p)
                              (if (eq? '+ p) "sum"
                                  (if (eq? '- p) "difference"
                                      (if (eq? '* p) "product"
                                          (if (eq? '= p) "numEqual"
                                              (if (eq? 'display p) "display"
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
                                          (string-append ", " (java-compile-formals (cdr formals)))
                                          "")))))
         (java-wrap-mutables (lambda (vars)
                               (if (not (pair? vars))
                                   ""
                                   (string-append
                                    (if (is-mutable? (car vars))
                                        (string-append
                                         " final ValueCell m_" (mangle (car vars))
                                         " = new ValueCell(" (mangle (car vars)) ");\n")
                                        "")
                                    (java-wrap-mutables (cdr vars))))))
         (java-compile-lambda (lambda (exp)
                                (let* ((formals (lambda->formals exp))
                                       (num-args (length formals)))
                                  (string-append
                                   "new NullProcValue" (number->string num-args) " () {\n"
                                   " public Value apply(" (java-compile-formals formals) ") {\n"
                                   (java-wrap-mutables formals)
                                   "\n"
                                   "  return " (java-compile-exp (lambda->exp exp)) " ;\n"
                                   "}}\n"))))
         (java-compile-args (lambda (args)
                              (if (not (pair? args))
                                  ""
                                  (string-append
                                   (java-compile-exp (car args))
                                   (if (pair? (cdr args))
                                       (string-append ", " (java-compile-args (cdr args)))
                                       "")))))
         (java-compile-set! (lambda (exp)
                              (string-append "VoidValue.Void(m_"
                                             (mangle (set!-var exp))
                                             ".value = "
                                             (java-compile-exp (set!-exp exp))
                                             ")")))
         (java-compile-app (lambda (exp)
                             (let* ((args     (app->args exp))
                                    (fun      (app->fun exp))
                                    (num-args (length args)))
                               (string-append
                                "((ProcValue" (number->string num-args) ")("
                                (java-compile-exp fun) ")).apply("
                                (java-compile-args args) ")\n"))))
         (java-compile-if (lambda (exp)
                            (string-append
                             "(" (java-compile-exp (if->condition exp)) ").toBoolean() ? ("
                             (java-compile-exp (if->then exp)) ") : ("
                             (java-compile-exp (if->else exp)) ")")))
         (java-compile-exp (lambda (exp)
                             (if (const? exp) (java-compile-const exp)
                                 (if (prim?  exp) (java-compile-prim exp)
                                     (if (ref?   exp) (java-compile-ref exp)
                                         (if (lambda? exp) (java-compile-lambda exp)
                                             (if (if? exp) (java-compile-if exp)
                                                 (if (set!? exp) (java-compile-set! exp)
                                                     (if (let? exp) (java-compile-exp (let=>lambda exp))
                                                         (if (letrec1? exp) (java-compile-exp (letrec1=>Y exp))
                                                             (if (begin? exp) (java-compile-exp (begin=>let exp))
                                                                 (if (app? exp) (java-compile-app exp)
                                                                     (error "no match")))))))))))))
         (java-compile-program (lambda (exp)
                                 (string-append
                                  "public class BOut extends RuntimeEnvironment {\n"
                                  " public static void main (String[] args) {\n"
                                  (java-compile-exp exp)
                                  " ;\n"
                                  " }\n"
                                  "}\n")))
         (input-program 3))
  (analyze-mutable-variables input-program)
  (java-compile-program input-program))
