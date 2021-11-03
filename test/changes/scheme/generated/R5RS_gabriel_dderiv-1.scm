; Changes:
; * removed: 2
; * added: 1
; * swaps: 3
; * negated predicates: 2
(letrec ((lookup (lambda (key table)
                   @sensitivity:FA
                   (letrec ((loop (lambda (x)
                                    @sensitivity:FA
                                    (if (<change> (null? x) (not (null? x)))
                                       #f
                                       (let ((pair (car x)))
                                          (if (eq? (car pair) key) pair (loop (cdr x))))))))
                      (loop table))))
         (properties ())
         (get (lambda (key1 key2)
                (<change>
                   @sensitivity:FA
                   ())
                (let ((x (lookup key1 properties)))
                   (if x
                      (let ((y (lookup key2 (cdr x))))
                         (if y (cdr y) #f))
                      #f))))
         (put (lambda (key1 key2 val)
                (<change>
                   @sensitivity:FA
                   (let ((x (lookup key1 properties)))
                      (if x
                         (let ((y (lookup key2 (cdr x))))
                            (if y
                               (set-cdr! y val)
                               (set-cdr! x (cons (cons key2 val) (cdr x)))))
                         (set! properties (cons (cons key1 (cons (cons key2 val) ())) properties)))))
                (<change>
                   (let ((x (lookup key1 properties)))
                      (if x
                         (let ((y (lookup key2 (cdr x))))
                            (if y
                               (set-cdr! y val)
                               (set-cdr! x (cons (cons key2 val) (cdr x)))))
                         (set! properties (cons (cons key1 (cons (cons key2 val) ())) properties))))
                   @sensitivity:FA)))
         (dderiv (lambda (a)
                   (<change>
                      ()
                      error)
                   @sensitivity:FA
                   (if (not (pair? a))
                      (if (eq? a 'x) 1 0)
                      (let ((f (get (car a) 'dderiv)))
                         (if (<change> f (not f))
                            (f a)
                            (error "No derivation method available"))))))
         (my+dderiv (lambda (a)
                      @sensitivity:FA
                      (cons '+ (map dderiv (cdr a)))))
         (my-dderiv (lambda (a)
                      @sensitivity:FA
                      (cons '- (map dderiv (cdr a)))))
         (*dderiv (lambda (a)
                    (<change>
                       @sensitivity:FA
                       ())
                    (cons
                       '*
                       (cons a (cons (cons '+ (map (lambda (a) (cons '/ (cons (dderiv a) (cons a ())))) (cdr a))) ())))))
         (/dderiv (lambda (a)
                    (<change>
                       @sensitivity:FA
                       (cons
                          '-
                          (cons
                             (cons '/ (cons (dderiv (cadr a)) (cons (caddr a) ())))
                             (cons
                                (cons
                                   '/
                                   (cons (cadr a) (cons (cons '* (cons (caddr a) (cons (caddr a) (cons (dderiv (caddr a)) ())))) ())))
                                ()))))
                    (<change>
                       (cons
                          '-
                          (cons
                             (cons '/ (cons (dderiv (cadr a)) (cons (caddr a) ())))
                             (cons
                                (cons
                                   '/
                                   (cons (cadr a) (cons (cons '* (cons (caddr a) (cons (caddr a) (cons (dderiv (caddr a)) ())))) ())))
                                ())))
                       @sensitivity:FA))))
   (put '+ 'dderiv my+dderiv)
   (put '- 'dderiv my-dderiv)
   (<change>
      (put '* 'dderiv *dderiv)
      (put '/ 'dderiv /dderiv))
   (<change>
      (put '/ 'dderiv /dderiv)
      (put '* 'dderiv *dderiv))
   (let ((arg (__toplevel_cons
                '+
                (__toplevel_cons
                   (__toplevel_cons '* (__toplevel_cons 3 (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                   (__toplevel_cons
                      (__toplevel_cons '* (__toplevel_cons 'a (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                      (__toplevel_cons
                         (__toplevel_cons '* (__toplevel_cons 'b (__toplevel_cons 'x ())))
                         (__toplevel_cons 5 ()))))))
         (result (__toplevel_cons
                   '+
                   (__toplevel_cons
                      (__toplevel_cons
                         '*
                         (__toplevel_cons
                            (__toplevel_cons '* (__toplevel_cons 3 (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                            (__toplevel_cons
                               (__toplevel_cons
                                  '+
                                  (__toplevel_cons
                                     (__toplevel_cons '/ (__toplevel_cons 0 (__toplevel_cons 3 ())))
                                     (__toplevel_cons
                                        (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ())))
                                        (__toplevel_cons (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ()))) ()))))
                               ())))
                      (__toplevel_cons
                         (__toplevel_cons
                            '*
                            (__toplevel_cons
                               (__toplevel_cons '* (__toplevel_cons 'a (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                               (__toplevel_cons
                                  (__toplevel_cons
                                     '+
                                     (__toplevel_cons
                                        (__toplevel_cons '/ (__toplevel_cons 0 (__toplevel_cons 'a ())))
                                        (__toplevel_cons
                                           (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ())))
                                           (__toplevel_cons (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ()))) ()))))
                                  ())))
                         (__toplevel_cons
                            (__toplevel_cons
                               '*
                               (__toplevel_cons
                                  (__toplevel_cons '* (__toplevel_cons 'b (__toplevel_cons 'x ())))
                                  (__toplevel_cons
                                     (__toplevel_cons
                                        '+
                                        (__toplevel_cons
                                           (__toplevel_cons '/ (__toplevel_cons 0 (__toplevel_cons 'b ())))
                                           (__toplevel_cons (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ()))) ())))
                                     ())))
                            (__toplevel_cons 0 ())))))))
      (equal? (dderiv arg) result)))