; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((all-but-interval (lambda (lst min max)
                             (letrec ((aux (lambda (last-smaller-cons aux-lst)
                                             (if (null? aux-lst)
                                                (set-cdr! last-smaller-cons ())
                                                (if (< (car aux-lst) min)
                                                   (aux aux-lst (cdr aux-lst))
                                                   (if (> (car aux-lst) max)
                                                      (set-cdr! last-smaller-cons aux-lst)
                                                      (aux last-smaller-cons (cdr aux-lst))))))))
                                (aux lst lst)
                                lst))))
   (<change>
      (if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 ())))))) 2 4) (__toplevel_cons 1 (__toplevel_cons 5 (__toplevel_cons 6 ()))))
         (if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))))) 2 2) (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ())))))
            (equal?
               (all-but-interval
                  (__toplevel_cons
                     1
                     (__toplevel_cons 2 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 ())))))
                  3
                  9)
               (__toplevel_cons 1 (__toplevel_cons 2 ())))
            #f)
         #f)
      ((lambda (x) x)
         (if (<change> (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 ())))))) 2 4) (__toplevel_cons 1 (__toplevel_cons 5 (__toplevel_cons 6 ())))) (not (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 ())))))) 2 4) (__toplevel_cons 1 (__toplevel_cons 5 (__toplevel_cons 6 ()))))))
            (if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))))) 2 2) (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ())))))
               (equal?
                  (all-but-interval
                     (__toplevel_cons
                        1
                        (__toplevel_cons 2 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 ())))))
                     3
                     9)
                  (__toplevel_cons 1 (__toplevel_cons 2 ())))
               #f)
            #f))))