; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((insert-aux! (lambda (lst lst2)
                        (set-cdr! lst2 ())
                        (<change>
                           (if (null? (cdr lst))
                              (set-cdr! lst lst2)
                              (insert-aux! (cdr lst) lst2))
                           ())
                        lst))
         (insert! (lambda (lst1 lst2)
                    (if (not (null? lst1))
                       (begin
                          (insert! (cdr lst1) (cdr lst2))
                          (insert-aux! (car lst1) lst2)
                          lst1)
                       #f))))
   (if (equal? (insert-aux! (__toplevel_cons 'a (__toplevel_cons 12 (__toplevel_cons 'q ()))) (__toplevel_cons 'v (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))) (__toplevel_cons 'a (__toplevel_cons 12 (__toplevel_cons 'q (__toplevel_cons 'v ())))))
      (equal?
         (insert!
            (__toplevel_cons
               (__toplevel_cons 'a (__toplevel_cons 12 (__toplevel_cons 'q ())))
               (__toplevel_cons
                  (__toplevel_cons 'b (__toplevel_cons 13 ()))
                  (__toplevel_cons
                     (__toplevel_cons 'c (__toplevel_cons 14 (__toplevel_cons 'r (__toplevel_cons 's ()))))
                     (__toplevel_cons
                        (__toplevel_cons 'f (__toplevel_cons 18 ()))
                        (__toplevel_cons (__toplevel_cons 'j (__toplevel_cons 22 (__toplevel_cons 't ()))) ())))))
            (__toplevel_cons
               'v
               (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))
         (__toplevel_cons
            (__toplevel_cons 'a (__toplevel_cons 12 (__toplevel_cons 'q (__toplevel_cons 'v ()))))
            (__toplevel_cons
               (__toplevel_cons 'b (__toplevel_cons 13 (__toplevel_cons 'w ())))
               (__toplevel_cons
                  (__toplevel_cons
                     'c
                     (__toplevel_cons 14 (__toplevel_cons 'r (__toplevel_cons 's (__toplevel_cons 'x ())))))
                  (__toplevel_cons
                     (__toplevel_cons 'f (__toplevel_cons 18 (__toplevel_cons 'y ())))
                     (__toplevel_cons
                        (__toplevel_cons 'j (__toplevel_cons 22 (__toplevel_cons 't (__toplevel_cons 'z ()))))
                        ()))))))
      #f))