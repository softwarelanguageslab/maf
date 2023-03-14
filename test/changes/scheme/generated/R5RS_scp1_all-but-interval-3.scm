; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((all-but-interval (lambda (lst min max)
                             (letrec ((aux (lambda (last-smaller-cons aux-lst)
                                             (if (null? aux-lst)
                                                (set-cdr! last-smaller-cons ())
                                                (if (< (car aux-lst) min)
                                                   (aux aux-lst (cdr aux-lst))
                                                   (if (> (car aux-lst) max)
                                                      (<change>
                                                         (set-cdr! last-smaller-cons aux-lst)
                                                         (aux last-smaller-cons (cdr aux-lst)))
                                                      (<change>
                                                         (aux last-smaller-cons (cdr aux-lst))
                                                         (set-cdr! last-smaller-cons aux-lst))))))))
                                (<change>
                                   (aux lst lst)
                                   lst)
                                (<change>
                                   lst
                                   (aux lst lst))))))
   (<change>
      ()
      (display
         (__toplevel_cons
            2
            (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 ())))))))
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
      #f))