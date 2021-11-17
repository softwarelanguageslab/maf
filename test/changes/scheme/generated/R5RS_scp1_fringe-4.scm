; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((atom? (lambda (x)
                  (not (pair? x))))
         (fringe (lambda (l)
                   (<change>
                      ()
                      (display null?))
                   (if (null? l)
                      ()
                      (if (atom? l)
                         (list l)
                         (append (fringe (car l)) (fringe (cdr l))))))))
   (<change>
      (equal?
         (fringe
            (__toplevel_cons
               (__toplevel_cons 1 ())
               (__toplevel_cons
                  (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 2 ()) ()) ()) ())
                  (__toplevel_cons
                     (__toplevel_cons
                        3
                        (__toplevel_cons (__toplevel_cons 4 (__toplevel_cons 5 ())) (__toplevel_cons 6 ())))
                     (__toplevel_cons
                        (__toplevel_cons (__toplevel_cons 7 ()) (__toplevel_cons 8 (__toplevel_cons 9 ())))
                        ())))))
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
                        (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ()))))))))))
      ((lambda (x) x)
         (equal?
            (fringe
               (__toplevel_cons
                  (__toplevel_cons 1 ())
                  (__toplevel_cons
                     (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 2 ()) ()) ()) ())
                     (__toplevel_cons
                        (__toplevel_cons
                           3
                           (__toplevel_cons (__toplevel_cons 4 (__toplevel_cons 5 ())) (__toplevel_cons 6 ())))
                        (__toplevel_cons
                           (__toplevel_cons (__toplevel_cons 7 ()) (__toplevel_cons 8 (__toplevel_cons 9 ())))
                           ())))))
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
                           (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ())))))))))))))