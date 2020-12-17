(define tagged-list? (lambda (l tag) (eq? (car l) tag))) ; Debug1 maar dan met andere indent lijn 42

(define (alphatize exp env) ; return a copy of 'exp' where each bound var has
  (define (alpha exp)       ; been renamed (to prevent aliasing problems)
    (cond ((symbol? exp)
           (let ((x (assq exp env))) (if x (cdr x) exp)))
          ((tagged-list? exp 'letrec)
           (let ((new-env (new-variables (map car (cadr exp)) env)))
             (list (car exp)
                   (map (lambda (x)
                          (list (cdr (assq (car x) new-env))
                                new-env))
                        (cadr exp))
                   new-env)))
          ((tagged-list? exp 'lambda)
             (list 'lambda
                   (map (lambda (x) (cdr (assq x env))) (cadr exp))
                   (alphatize (caddr exp) env)))
          (else exp)))
  (alpha exp))

(define (new-variables parms env)
  (append (map (lambda (x) (cons x (string->symbol (string-append (symbol->string x))))) parms) env))

(define (peval proc args)
  (simplify!
      (list 'lambda
            (cadr proc) ; remove the constant parameters
              (caddr proc))))

(define (simplify! exp)
  (define (simp! where env)

    (define (s! where)
      (let ((exp (car where)))

        (cond ((tagged-list? exp 'begin)
               (for-each s! (cdr exp)))
              ((symbol? (car exp)) ; is the operator position a var ref?
                      (let ((frame (binding-frame (car exp) env)))
                        (set-car! where #f)))
                     ((eq? (caar exp) 'lambda) ; <= Changing the indentation changes the result.
                      (set-car! where
                        (list 'let
                              (map (map (lambda (x) (lambda (y) (list x y))) (cadar exp)) (cdr exp))
                              (caddar exp)))
                      (s! where)))))
    (s! where))
    (simp! (list exp) '()))

(define (binding-frame var env)
  (if (assq var (cadar env)) (car env) (binding-frame var (cdr env))))

(peval (alphatize
     '(lambda ()
          (letrec ((copy (lambda ())))
            (copy)))
     '())
 '())
