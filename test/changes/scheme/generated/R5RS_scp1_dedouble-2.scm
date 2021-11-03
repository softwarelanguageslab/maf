; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 0
(letrec ((ontdubbel! (lambda (lijst)
                       (let ((deEven ())
                             (deOneven ()))
                          (letrec ((ontdubbel-iter (lambda (prevE prevO restLijst)
                                                     (<change>
                                                        ()
                                                        (display ()))
                                                     (<change>
                                                        ()
                                                        prevE)
                                                     (if (null? restLijst)
                                                        (begin
                                                           (set-cdr! prevE ())
                                                           (<change>
                                                              (set-cdr! prevO ())
                                                              (cons deEven deOneven))
                                                           (<change>
                                                              (cons deEven deOneven)
                                                              (set-cdr! prevO ())))
                                                        (if (even? (car restLijst))
                                                           (begin
                                                              (if (null? prevE)
                                                                 (set! deEven restLijst)
                                                                 (set-cdr! prevE restLijst))
                                                              (ontdubbel-iter restLijst prevO (cdr restLijst)))
                                                           (begin
                                                              (if (null? prevO)
                                                                 (set! deOneven restLijst)
                                                                 (set-cdr! prevO restLijst))
                                                              (ontdubbel-iter prevE restLijst (cdr restLijst))))))))
                             (ontdubbel-iter deEven deOneven lijst))))))
   (equal?
      (ontdubbel!
         (__toplevel_cons
            1
            (__toplevel_cons
               2
               (__toplevel_cons
                  3
                  (__toplevel_cons
                     4
                     (__toplevel_cons
                        5
                        (__toplevel_cons
                           6
                           (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 10 ())))))))))))
      (__toplevel_cons
         (__toplevel_cons
            2
            (__toplevel_cons 4 (__toplevel_cons 6 (__toplevel_cons 8 (__toplevel_cons 10 ())))))
         (__toplevel_cons
            1
            (__toplevel_cons 3 (__toplevel_cons 5 (__toplevel_cons 7 (__toplevel_cons 9 ()))))))))