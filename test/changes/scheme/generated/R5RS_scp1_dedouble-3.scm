; Changes:
; * removed: 2
; * added: 0
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((ontdubbel! (lambda (lijst)
                       (let ((deEven ())
                             (deOneven ()))
                          (letrec ((ontdubbel-iter (lambda (prevE prevO restLijst)
                                                     (if (null? restLijst)
                                                        (begin
                                                           (set-cdr! prevE ())
                                                           (<change>
                                                              (set-cdr! prevO ())
                                                              ())
                                                           (cons deEven deOneven))
                                                        (if (<change> (even? (car restLijst)) (not (even? (car restLijst))))
                                                           (begin
                                                              (<change>
                                                                 (if (null? prevE)
                                                                    (set! deEven restLijst)
                                                                    (set-cdr! prevE restLijst))
                                                                 ())
                                                              (ontdubbel-iter restLijst prevO (cdr restLijst)))
                                                           (begin
                                                              (<change>
                                                                 (if (null? prevO)
                                                                    (set! deOneven restLijst)
                                                                    (set-cdr! prevO restLijst))
                                                                 (ontdubbel-iter prevE restLijst (cdr restLijst)))
                                                              (<change>
                                                                 (ontdubbel-iter prevE restLijst (cdr restLijst))
                                                                 (if (null? prevO)
                                                                    (set! deOneven restLijst)
                                                                    (set-cdr! prevO restLijst)))))))))
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