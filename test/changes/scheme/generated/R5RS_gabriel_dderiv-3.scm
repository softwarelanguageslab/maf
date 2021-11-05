; Changes:
; * removed: 0
; * added: 5
; * swaps: 1
; * negated predicates: 2
; * swapped branches: 0
; * calls to id fun: 3
(letrec ((lookup (lambda (key table)
                   @sensitivity:FA
                   (letrec ((loop (lambda (x)
                                    @sensitivity:FA
                                    (if (null? x)
                                       #f
                                       (let ((pair (car x)))
                                          (if (<change> (eq? (car pair) key) (not (eq? (car pair) key)))
                                             pair
                                             (loop (cdr x))))))))
                      (loop table))))
         (properties ())
         (get (lambda (key1 key2)
                (<change>
                   ()
                   key2)
                (<change>
                   ()
                   cdr)
                @sensitivity:FA
                (let ((x (lookup key1 properties)))
                   (if x
                      (let ((y (lookup key2 (cdr x))))
                         (if y (cdr y) #f))
                      #f))))
         (put (lambda (key1 key2 val)
                @sensitivity:FA
                (let ((x (lookup key1 properties)))
                   (if x
                      (let ((y (lookup key2 (cdr x))))
                         (<change>
                            (if y
                               (set-cdr! y val)
                               (set-cdr! x (cons (cons key2 val) (cdr x))))
                            ((lambda (x) x) (if y (set-cdr! y val) (set-cdr! x (cons (cons key2 val) (cdr x)))))))
                      (set! properties (cons (cons key1 (cons (cons key2 val) ())) properties))))))
         (dderiv (lambda (a)
                   (<change>
                      @sensitivity:FA
                      (if (not (not (pair? a)))
                         (if (eq? a 'x) 1 0)
                         (let ((f (get (car a) 'dderiv)))
                            (if f
                               (f a)
                               (error "No derivation method available")))))
                   (<change>
                      (if (not (pair? a))
                         (if (eq? a 'x) 1 0)
                         (let ((f (get (car a) 'dderiv)))
                            (if f
                               (f a)
                               (error "No derivation method available"))))
                      @sensitivity:FA)))
         (my+dderiv (lambda (a)
                      (<change>
                         ()
                         dderiv)
                      @sensitivity:FA
                      (cons '+ (map dderiv (cdr a)))))
         (my-dderiv (lambda (a)
                      @sensitivity:FA
                      (<change>
                         (cons '- (map dderiv (cdr a)))
                         ((lambda (x) x) (cons '- (map dderiv (cdr a)))))))
         (*dderiv (lambda (a)
                    @sensitivity:FA
                    (cons
                       '*
                       (cons
                          a
                          (cons
                             (cons
                                '+
                                (map
                                   (lambda (a)
                                      (<change>
                                         (cons '/ (cons (dderiv a) (cons a ())))
                                         ((lambda (x) x) (cons '/ (cons (dderiv a) (cons a ()))))))
                                   (cdr a)))
                             ())))))
         (/dderiv (lambda (a)
                    (<change>
                       ()
                       cons)
                    @sensitivity:FA
                    (<change>
                       ()
                       cons)
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
   (put '* 'dderiv *dderiv)
   (put '/ 'dderiv /dderiv)
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