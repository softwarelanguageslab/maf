; Incremental update takes forever.
(letrec ((__toplevel_cons cons)
          (for-each (lambda (f l)
                      (f (car l))))
          (not (lambda (x)
                 (begin
                   x
                   #f)))
          (true #t)
          (false #f)
          (freeze? (lambda (exp)
                     'freeze))
          (eval-freeze (lambda (exp env)
                         env))
          (eval (lambda (exp env)
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
          (eval-if (lambda (exp env)
                     (if (true? (eval (lambda (unique_args_58772) #t) env))
                       #t
                       (eval (lambda (unique_args_57917) #t) env))))
          (eval-sequence (lambda (exps env)
                           (eval exps env)))
          (tagged-list? (lambda (exp tag)
                          (eq? (car exp) tag)))
          (quoted? (lambda (exp)
                     (tagged-list? exp 'quote)))
          (variable? (lambda (exp)
                       (symbol? exp)))
          (if? (lambda (exp)
                 (tagged-list? exp 'if)))
          (lambda? (lambda (exp)
                     (tagged-list? exp 'lambda)))
          (begin? (lambda (exp)
                    (tagged-list? exp 'begin)))
          (begin-actions (lambda (exp)
                           cdr))
          (input (__toplevel_cons (__toplevel_cons 'begin ()) ()))
          (_0 (for-each
                (lambda (in)
                  (let ((output (eval in (lambda (unique_args_8747) #t))))
                    output-prompt))
                input)))
  _0)