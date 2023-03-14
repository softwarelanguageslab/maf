#lang racket/base
(require soft-contract/fake-contract)

;; A closure-compiling Scheme interpreter running
;;  a Y-combinator countdown

(define (find-pos a l)
  (cond
    ((eq? a (car l)) 0)
    (else (+ 1 (find-pos a (cdr l))))))

(define (comp expr cenv)
  (cond
    ((number? expr) (lambda (env) expr))
    ((symbol? expr)
     (let ((pos (find-pos expr cenv)))
       (case pos
         ((0) car)
         ((1) cadr)
         (else
          (lambda (env)
            (list-ref env pos))))))
    (else
     (case (car expr)
       ((+)
        (let ((a (comp (cadr expr) cenv))
              (b (comp (caddr expr) cenv)))
          (lambda (env) (+ (a env) (b env)))))
       ((-)
        (let ((a (comp (cadr expr) cenv))
              (b (comp (caddr expr) cenv)))
          (lambda (env) (- (a env) (b env)))))
       ((zero?)
        (let ((a (comp (cadr expr) cenv)))
          (lambda (env) (zero? (a env)))))
       ((if)
        (let ((a (comp (cadr expr) cenv))
              (b (comp (caddr expr) cenv))
              (c (comp (cadddr expr) cenv)))
          (lambda (env)
            (if (a env)
                (b env)
                (c env)))))
       ((let)
        (let ((rhs (comp (cadr (caadr expr)) cenv))
              (body (comp (caddr expr)
                          (cons (car (caadr expr))
                                cenv))))
          (lambda (env)
            (let ((rhs-val (rhs env)))
              (body (cons rhs-val env))))))
       ((lambda)
        (let ((body (comp (caddr expr)
                          (cons (caadr expr) cenv))))
          (lambda (env)
            (lambda (a)
              (body (cons a env))))))
       (else
        (let ((a (comp (car expr) cenv))
              (b (comp (cadr expr) cenv)))
          (lambda (env)
            (let ((clos (a env))
                  (arg-val (b env)))
              (clos arg-val)))))))))

((comp '(let ((f (lambda (x) 
                   (lambda (y)
                     (- y x))))) 
          (+ ((f 10) 2) 3))
       '())
 '())

(provide
 (contract-out
  [comp (any/c . -> . any/c #:total? #t)]))
