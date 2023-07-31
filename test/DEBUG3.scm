; Incremental update takes forever.
(define __toplevel_cons cons)
(define for-each (lambda (f l)
                   (f (car l))))
(define not (lambda (x)
              (begin
                x
                #f)))
(define true #t)
(define false #f)
(define freeze? (lambda (exp)
                  'freeze))
(define eval-freeze (lambda (exp env)
                      env))
(define eval (lambda (exp env)
               (if (variable? exp)
                 #t
                 (begin
                   (quoted? exp)
                   (if #f
                     (lambda (unique_args_65663)
                       #t)
                     (if #f
                       (lambda (unique_args_64946)
                         0)
                       (if (if? exp)
                         (eval-if exp env)
                         (if (lambda? exp)
                           #f
                           (if (begin? exp)
                             (eval-sequence begin-actions env)
                             (<change>
                               #f
                               (freeze? exp)))))))))))
(define eval-if (lambda (exp env)
                  (if (true? (eval (lambda (unique_args_58772) #t) env))
                    #t
                    (eval (lambda (unique_args_57917) #t) env))))
(define eval-sequence (lambda (exps env)
                        (eval exps env)))
(define tagged-list? (lambda (exp tag)
                       (eq? (car exp) tag)))
(define quoted? (lambda (exp)
                  (tagged-list? exp 'quote)))
(define variable? (lambda (exp)
                    (symbol? exp)))
(define if? (lambda (exp)
              (tagged-list? exp 'if)))
(define lambda? (lambda (exp)
                  (tagged-list? exp 'lambda)))
(define begin? (lambda (exp)
                 (tagged-list? exp 'begin)))
(define begin-actions (lambda (exp)
                        cdr))
(define input (__toplevel_cons (__toplevel_cons 'begin '()) '()))
(for-each
  (lambda (in)
    (let ((output (eval in (lambda (unique_args_8747) #t))))
      output-prompt))
  input)