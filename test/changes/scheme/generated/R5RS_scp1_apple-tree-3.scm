; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 2
; * calls to id fun: 5
(letrec ((boom (__toplevel_cons
                 (__toplevel_cons 'blad (__toplevel_cons (__toplevel_cons 'appel 'golden) ()))
                 (__toplevel_cons
                    (__toplevel_cons 'blad (__toplevel_cons (__toplevel_cons 'appel 'granny) ()))
                    (__toplevel_cons
                       (__toplevel_cons
                          (__toplevel_cons (__toplevel_cons 'appel 'golden) (__toplevel_cons 'blad ()))
                          (__toplevel_cons 'blad (__toplevel_cons (__toplevel_cons 'appel 'cox) ())))
                       ()))))
         (blad? (lambda (boom)
                  (<change>
                     (eq? boom 'blad)
                     ((lambda (x) x) (eq? boom 'blad)))))
         (appel? (lambda (boom)
                   (if (pair? boom) (eq? (car boom) 'appel) #f)))
         (type (lambda (appel)
                 (cdr appel)))
         (leafs (lambda (boom)
                  (if (null? boom)
                     0
                     (if (blad? boom)
                        1
                        (if (appel? boom)
                           0
                           (+ (leafs (car boom)) (leafs (cdr boom))))))))
         (all-apples (lambda (boom)
                       (<change>
                          (if (null? boom)
                             ()
                             (if (blad? boom)
                                ()
                                (if (appel? boom)
                                   (list (type boom))
                                   (append (all-apples (car boom)) (all-apples (cdr boom))))))
                          ((lambda (x) x)
                             (if (null? boom)
                                (<change>
                                   ()
                                   (if (blad? boom)
                                      ()
                                      (if (appel? boom)
                                         (list (type boom))
                                         (append (all-apples (car boom)) (all-apples (cdr boom))))))
                                (<change>
                                   (if (blad? boom)
                                      ()
                                      (if (appel? boom)
                                         (list (type boom))
                                         (append (all-apples (car boom)) (all-apples (cdr boom)))))
                                   ()))))))
         (conditional-append (lambda (l1 l2)
                               (if (null? l1)
                                  l2
                                  (if (member (car l1) l2)
                                     (conditional-append (cdr l1) l2)
                                     (cons (car l1) (conditional-append (cdr l1) l2))))))
         (apple-types (lambda (boom)
                        (if (null? boom)
                           ()
                           (if (blad? boom)
                              ()
                              (if (appel? boom)
                                 (list (type boom))
                                 (conditional-append (apple-types (car boom)) (apple-types (cdr boom))))))))
         (bewerk-boom (lambda (boom doe-blad doe-appel combiner init)
                        (if (null? boom)
                           init
                           (if (blad? boom)
                              (<change>
                                 (doe-blad boom)
                                 (if (not (appel? boom))
                                    (doe-appel boom)
                                    (combiner
                                       (bewerk-boom (car boom) doe-blad doe-appel combiner init)
                                       (bewerk-boom (cdr boom) doe-blad doe-appel combiner init))))
                              (<change>
                                 (if (appel? boom)
                                    (doe-appel boom)
                                    (combiner
                                       (bewerk-boom (car boom) doe-blad doe-appel combiner init)
                                       (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))
                                 (doe-blad boom))))))
         (leafs-dmv-bewerk (lambda (boom)
                             (bewerk-boom boom (lambda (blad) 1) (lambda (appel) (<change> 0 ((lambda (x) x) 0))) + 0)))
         (all-apples-dmv-bewerk (lambda (boom)
                                  (bewerk-boom
                                     boom
                                     (lambda (blad)
                                        (<change>
                                           ()
                                           (display ()))
                                        (<change>
                                           ()
                                           ((lambda (x) x) ())))
                                     (lambda (appel)
                                        (list (type appel)))
                                     append
                                     ())))
         (apple-types-dmv-bewerk (lambda (boom)
                                   (bewerk-boom
                                      boom
                                      (lambda (blad)
                                         ())
                                      (lambda (appel)
                                         (<change>
                                            (list (type appel))
                                            ((lambda (x) x) (list (type appel)))))
                                      conditional-append
                                      ()))))
   (if (<change> (= (leafs boom) 4) (not (= (leafs boom) 4)))
      (if (equal? (all-apples boom) (__toplevel_cons 'golden (__toplevel_cons 'granny (__toplevel_cons 'golden (__toplevel_cons 'cox ())))))
         (if (equal? (apple-types boom) (__toplevel_cons 'granny (__toplevel_cons 'golden (__toplevel_cons 'cox ()))))
            (if (= (leafs-dmv-bewerk boom) 4)
               (if (equal? (all-apples-dmv-bewerk boom) (__toplevel_cons 'golden (__toplevel_cons 'granny (__toplevel_cons 'golden (__toplevel_cons 'cox ())))))
                  (equal?
                     (apple-types-dmv-bewerk boom)
                     (__toplevel_cons 'granny (__toplevel_cons 'golden (__toplevel_cons 'cox ()))))
                  #f)
               #f)
            #f)
         #f)
      #f))