; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((atom? (lambda (x)
                  (not (pair? x))))
         (fringe (lambda (l)
                   (if (<change> (null? l) (not (null? l)))
                      ()
                      (if (atom? l)
                         (list l)
                         (append (fringe (car l)) (fringe (cdr l))))))))
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
                     (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ())))))))))))