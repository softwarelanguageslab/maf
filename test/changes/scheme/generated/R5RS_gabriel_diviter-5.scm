; Changes:
; * removed: 0
; * added: 1
; * swaps: 3
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((create-n (lambda (n)
                     @sensitivity:FA
                     (<change>
                        ()
                        @sensitivity:FA)
                     (letrec ((__do_loop (lambda (n a)
                                           (<change>
                                              @sensitivity:FA
                                              (if (= n 0) a (__do_loop (- n 1) (cons () a))))
                                           (<change>
                                              (if (= n 0) a (__do_loop (- n 1) (cons () a)))
                                              @sensitivity:FA))))
                        (<change>
                           (__do_loop n ())
                           ((lambda (x) x) (__do_loop n ()))))))
         (*ll* (create-n 200))
         (iterative-div2 (lambda (l)
                           (<change>
                              @sensitivity:FA
                              (letrec ((__do_loop (lambda (l a)
                                                    (if (null? l)
                                                       a
                                                       (__do_loop (cddr l) (cons (car l) a)))
                                                    @sensitivity:FA)))
                                 (__do_loop l ())))
                           (<change>
                              (letrec ((__do_loop (lambda (l a)
                                                    @sensitivity:FA
                                                    (if (null? l)
                                                       a
                                                       (__do_loop (cddr l) (cons (car l) a))))))
                                 (__do_loop l ()))
                              @sensitivity:FA))))
   (equal?
      (iterative-div2 *ll*)
      (__toplevel_cons
         ()
         (__toplevel_cons
            ()
            (__toplevel_cons
               ()
               (__toplevel_cons
                  ()
                  (__toplevel_cons
                     ()
                     (__toplevel_cons
                        ()
                        (__toplevel_cons
                           ()
                           (__toplevel_cons
                              ()
                              (__toplevel_cons
                                 ()
                                 (__toplevel_cons
                                    ()
                                    (__toplevel_cons
                                       ()
                                       (__toplevel_cons
                                          ()
                                          (__toplevel_cons
                                             ()
                                             (__toplevel_cons
                                                ()
                                                (__toplevel_cons
                                                   ()
                                                   (__toplevel_cons
                                                      ()
                                                      (__toplevel_cons
                                                         ()
                                                         (__toplevel_cons
                                                            ()
                                                            (__toplevel_cons
                                                               ()
                                                               (__toplevel_cons
                                                                  ()
                                                                  (__toplevel_cons
                                                                     ()
                                                                     (__toplevel_cons
                                                                        ()
                                                                        (__toplevel_cons
                                                                           ()
                                                                           (__toplevel_cons
                                                                              ()
                                                                              (__toplevel_cons
                                                                                 ()
                                                                                 (__toplevel_cons
                                                                                    ()
                                                                                    (__toplevel_cons
                                                                                       ()
                                                                                       (__toplevel_cons
                                                                                          ()
                                                                                          (__toplevel_cons
                                                                                             ()
                                                                                             (__toplevel_cons
                                                                                                ()
                                                                                                (__toplevel_cons
                                                                                                   ()
                                                                                                   (__toplevel_cons
                                                                                                      ()
                                                                                                      (__toplevel_cons
                                                                                                         ()
                                                                                                         (__toplevel_cons
                                                                                                            ()
                                                                                                            (__toplevel_cons
                                                                                                               ()
                                                                                                               (__toplevel_cons
                                                                                                                  ()
                                                                                                                  (__toplevel_cons
                                                                                                                     ()
                                                                                                                     (__toplevel_cons
                                                                                                                        ()
                                                                                                                        (__toplevel_cons
                                                                                                                           ()
                                                                                                                           (__toplevel_cons
                                                                                                                              ()
                                                                                                                              (__toplevel_cons
                                                                                                                                 ()
                                                                                                                                 (__toplevel_cons
                                                                                                                                    ()
                                                                                                                                    (__toplevel_cons
                                                                                                                                       ()
                                                                                                                                       (__toplevel_cons
                                                                                                                                          ()
                                                                                                                                          (__toplevel_cons
                                                                                                                                             ()
                                                                                                                                             (__toplevel_cons
                                                                                                                                                ()
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   ()
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      ()
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         ()
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            ()
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               ()
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  ()
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     ()
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        ()
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           ()
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              ()
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 ()
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    ()
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       ()
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          ()
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             ()
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                ()
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   ()
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      ()
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         ()
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            ()
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               ()
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 ()
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    ()
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       ()
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          ()
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             ()
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      ()
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         ()
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            ()
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               ()
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 ()
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    ()
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       ()
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          ()
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             ()
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      ()
                                                                                                                                                                                                                                                                                                      (__toplevel_cons () (__toplevel_cons () (__toplevel_cons () (__toplevel_cons () ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))