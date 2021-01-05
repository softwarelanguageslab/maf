(define (tagged-list? l tag) (eq? (car l) tag)) ; verdere reductie debug2
(define (as k l)
  (eq? (caar l) k))

(define (alphatize exp)
    (cond ((tagged-list? exp 'letrec)
           (let ((new-env (new-variables (map car (cadr exp)))))
             (list (car exp)
                   (cadr exp)
                   new-env)))
          ((tagged-list? exp 'lambda)
             (list 'lambda
                   (alphatize (caddr exp))))))

(define (new-variables parms)
  (map (lambda (x) (cons x x)) parms))

(define (peval proc)
  (simplify!
      (list 'lambda
              (caddr proc))))

(define (simplify! exp)
  (define (simp! where)
    (define (s! where)
      (let ((exp (car where)))
        (cond ((tagged-list? (car where) 'begin) ; Unuseful clause...
               (for-each s! (car where)))
              ((symbol? (car (car where)))
                 (as (car (car where)) (cadr '()))) ; <<<===== ERROR
              ((<change> (eq? (caar exp) 'lambda) (tagged-list? (car exp) 'lambda)))))) ; This dependency is only removed if caar is replaced by car car on line 3.
    (s! where))
  (simp! (list exp)))

(peval (alphatize
     '(lambda ()
          (letrec ((copy (lambda ())))
            (copy)))))
