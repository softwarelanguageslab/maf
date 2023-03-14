; Changes:
; * removed: 3
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 5
(letrec ((lookup (lambda (key table)
                   @sensitivity:FA
                   (letrec ((loop (lambda (x)
                                    (<change>
                                       ()
                                       (display key))
                                    (<change>
                                       @sensitivity:FA
                                       ())
                                    (<change>
                                       (if (null? x)
                                          #f
                                          (let ((pair (car x)))
                                             (if (eq? (car pair) key) pair (loop (cdr x)))))
                                       ((lambda (x) x)
                                          (if (null? x)
                                             #f
                                             (let ((pair (car x)))
                                                (if (eq? (car pair) key) pair (loop (cdr x))))))))))
                      (loop table))))
         (properties ())
         (get (lambda (key1 key2)
                (<change>
                   @sensitivity:FA
                   ())
                (<change>
                   (let ((x (lookup key1 properties)))
                      (if x
                         (let ((y (lookup key2 (cdr x))))
                            (if y (cdr y) #f))
                         #f))
                   ((lambda (x) x)
                      (let ((x (lookup key1 properties)))
                         (if x
                            (let ((y (lookup key2 (cdr x))))
                               (if y (cdr y) #f))
                            #f))))))
         (put (lambda (key1 key2 val)
                @sensitivity:FA
                (<change>
                   (let ((x (lookup key1 properties)))
                      (if x
                         (let ((y (lookup key2 (cdr x))))
                            (if y
                               (set-cdr! y val)
                               (set-cdr! x (cons (cons key2 val) (cdr x)))))
                         (set! properties (cons (cons key1 (cons (cons key2 val) ())) properties))))
                   ((lambda (x) x)
                      (let ((x (lookup key1 properties)))
                         (if x
                            (let ((y (lookup key2 (cdr x))))
                               (if y
                                  (<change>
                                     (set-cdr! y val)
                                     (set-cdr! x (cons (cons key2 val) (cdr x))))
                                  (<change>
                                     (set-cdr! x (cons (cons key2 val) (cdr x)))
                                     (set-cdr! y val))))
                            (set! properties (cons (cons key1 (cons (cons key2 val) ())) properties))))))))
         (dderiv (lambda (a)
                   @sensitivity:FA
                   (if (not (pair? a))
                      (if (eq? a 'x) 1 0)
                      (let ((f (get (car a) 'dderiv)))
                         (if f
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
                    (<change>
                       (cons
                          '*
                          (cons a (cons (cons '+ (map (lambda (a) (cons '/ (cons (dderiv a) (cons a ())))) (cdr a))) ())))
                       ((lambda (x) x)
                          (cons
                             '*
                             (cons a (cons (cons '+ (map (lambda (a) (cons '/ (cons (dderiv a) (cons a ())))) (cdr a))) ())))))))
         (/dderiv (lambda (a)
                    @sensitivity:FA
                    (cons
                       '-
                       (cons
                          (cons '/ (cons (dderiv (cadr a)) (cons (caddr a) ())))
                          (cons
                             (cons
                                '/
                                (cons (cadr a) (cons (cons '* (cons (caddr a) (cons (caddr a) (cons (dderiv (caddr a)) ())))) ())))
                             ()))))))
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
      (<change>
         (equal? (dderiv arg) result)
         ((lambda (x) x) (equal? (dderiv arg) result)))))