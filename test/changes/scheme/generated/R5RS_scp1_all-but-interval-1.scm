; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((all-but-interval (lambda (lst min max)
                             (<change>
                                ()
                                cdr)
                             (letrec ((aux (lambda (last-smaller-cons aux-lst)
                                             (if (null? aux-lst)
                                                (set-cdr! last-smaller-cons ())
                                                (if (< (car aux-lst) min)
                                                   (aux aux-lst (cdr aux-lst))
                                                   (if (<change> (> (car aux-lst) max) (not (> (car aux-lst) max)))
                                                      (set-cdr! last-smaller-cons aux-lst)
                                                      (aux last-smaller-cons (cdr aux-lst))))))))
                                (aux lst lst)
                                lst))))
   (if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 ())))))) 2 4) (__toplevel_cons 1 (__toplevel_cons 5 (__toplevel_cons 6 ()))))
      (if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))))) 2 2) (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ())))))
         (<change>
            (equal?
               (all-but-interval
                  (__toplevel_cons
                     1
                     (__toplevel_cons 2 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 ())))))
                  3
                  9)
               (__toplevel_cons 1 (__toplevel_cons 2 ())))
            #f)
         (<change>
            #f
            (equal?
               (all-but-interval
                  (__toplevel_cons
                     1
                     (__toplevel_cons 2 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 ())))))
                  3
                  9)
               (__toplevel_cons 1 (__toplevel_cons 2 ())))))
      #f))