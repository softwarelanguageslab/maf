(define (tagged-list? l tag) (eq? (car l) tag)) ; verdere reductie debug2

(define (alphatize exp env)
    (cond ((tagged-list? exp 'letrec)
           (let ((new-env (new-variables (map car (cadr exp)))))
             (list (car exp)
                   (cadr exp)
                   new-env)))
          ((tagged-list? exp 'lambda)
             (list 'lambda
                   (cadr exp)
                   (alphatize (caddr exp) env)))))

(define (new-variables parms)
  (map (lambda (x) (cons x x)) parms))

(define (peval proc args)
  (simplify!
      (list 'lambda
            (cadr proc)
              (caddr proc))))

(define (simplify! exp)
  (define (simp! where env)
    (define (s! where)
      (let ((exp (car where)))
        (cond ((tagged-list? exp 'begin)
               (for-each s! exp))
              ((symbol? (car exp))
                 (let ((frame (binding-frame (car exp) env))) #t)) ; Can't remove this let...
              ((<change> (eq? (caar exp) 'lambda) (tagged-list? (car exp) 'lambda))))))
    (s! where))
    (simp! (list exp) '()))

(define (binding-frame var env)
  (if (assq var (cadr env)) '() '())) ; Cannot remove this if test...

(peval (alphatize
     '(lambda ()
          (letrec ((copy (lambda ())))
            (copy)))
     '())
 '())
