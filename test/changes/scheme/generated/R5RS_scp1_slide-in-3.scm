; Changes:
; * removed: 1
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((schuif-in! (lambda (l1 l2)
                       (if (null? (cdr l1))
                          (begin
                             (<change>
                                (set-cdr! l1 l2)
                                ())
                             (<change>
                                ()
                                (set-cdr! l1 l2))
                             'ok)
                          (if (null? l2)
                             (<change>
                                'ok
                                (let ((rest1 (cdr l1))
                                      (rest2 (cdr l2)))
                                   (set-cdr! l1 l2)
                                   (set-cdr! l2 rest1)
                                   (schuif-in! rest1 rest2)))
                             (<change>
                                (let ((rest1 (cdr l1))
                                      (rest2 (cdr l2)))
                                   (set-cdr! l1 l2)
                                   (set-cdr! l2 rest1)
                                   (schuif-in! rest1 rest2))
                                'ok)))))
         (lijst1 (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 5 ()))))
         (lijst2 (__toplevel_cons 2 (__toplevel_cons 4 (__toplevel_cons 6 (__toplevel_cons 8 ()))))))
   (<change>
      ()
      lijst1)
   (schuif-in! lijst1 lijst2)
   (equal?
      lijst1
      (__toplevel_cons
         1
         (__toplevel_cons
            2
            (__toplevel_cons
               3
               (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 8 ())))))))))