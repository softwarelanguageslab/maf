; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((schuif-in! (lambda (l1 l2)
                       (if (null? (cdr l1))
                          (begin
                             (set-cdr! l1 l2)
                             'ok)
                          (if (null? l2)
                             'ok
                             (let ((rest1 (cdr l1))
                                   (rest2 (cdr l2)))
                                (set-cdr! l1 l2)
                                (<change>
                                   (set-cdr! l2 rest1)
                                   (schuif-in! rest1 rest2))
                                (<change>
                                   (schuif-in! rest1 rest2)
                                   (set-cdr! l2 rest1)))))))
         (lijst1 (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 5 ()))))
         (lijst2 (__toplevel_cons 2 (__toplevel_cons 4 (__toplevel_cons 6 (__toplevel_cons 8 ()))))))
   (<change>
      (schuif-in! lijst1 lijst2)
      ())
   (equal?
      lijst1
      (__toplevel_cons
         1
         (__toplevel_cons
            2
            (__toplevel_cons
               3
               (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 8 ())))))))))