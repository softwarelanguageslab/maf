; Changes:
; * removed: 2
; * added: 5
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((lookup (lambda (key table)
                   @sensitivity:FA
                   (letrec ((loop (lambda (x)
                                    (<change>
                                       @sensitivity:FA
                                       ())
                                    (if (null? x)
                                       #f
                                       (let ((pair (car x)))
                                          (if (eq? (car pair) key) pair (loop (cdr x))))))))
                      (<change>
                         ()
                         loop)
                      (loop table))))
         (properties ())
         (get (lambda (key1 key2)
                @sensitivity:FA
                (let ((x (lookup key1 properties)))
                   (if x
                      (let ((y (lookup key2 (cdr x))))
                         (if y (cdr y) #f))
                      #f))))
         (put (lambda (key1 key2 val)
                @sensitivity:FA
                (<change>
                   ()
                   key1)
                (let ((x (lookup key1 properties)))
                   (if x
                      (let ((y (lookup key2 (cdr x))))
                         (if y
                            (set-cdr! y val)
                            (set-cdr! x (cons (cons key2 val) (cdr x)))))
                      (set! properties (cons (cons key1 (cons (cons key2 val) ())) properties))))))
         (dderiv (lambda (a)
                   @sensitivity:FA
                   (<change>
                      ()
                      error)
                   (<change>
                      ()
                      a)
                   (if (not (pair? a))
                      (if (<change> (eq? a 'x) (not (eq? a 'x))) 1 0)
                      (let ((f (get (car a) 'dderiv)))
                         (if f
                            (f a)
                            (error "No derivation method available"))))))
         (my+dderiv (lambda (a)
                      (<change>
                         @sensitivity:FA
                         ((lambda (x) x) @sensitivity:FA))
                      (cons '+ (map dderiv (cdr a)))))
         (my-dderiv (lambda (a)
                      (<change>
                         @sensitivity:FA
                         ())
                      (cons '- (map dderiv (cdr a)))))
         (*dderiv (lambda (a)
                    @sensitivity:FA
                    (cons
                       '*
                       (cons a (cons (cons '+ (map (lambda (a) (cons '/ (cons (dderiv a) (cons a ())))) (cdr a))) ())))))
         (/dderiv (lambda (a)
                    @sensitivity:FA
                    (<change>
                       ()
                       caddr)
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
   (<change>
      (put '/ 'dderiv /dderiv)
      ((lambda (x) x) (put '/ 'dderiv /dderiv)))
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