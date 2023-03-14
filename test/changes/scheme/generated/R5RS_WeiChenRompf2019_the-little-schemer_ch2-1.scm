; Changes:
; * removed: 2
; * added: 0
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((atom? (lambda (x)
                  (if (not (pair? x)) (not (null? x)) #f)))
         (lat? (lambda (l)
                 (if (null? l)
                    #t
                    (if (atom? (car l)) (lat? (cdr l)) #f)))))
   (lat?
      (__toplevel_cons
         'Jack
         (__toplevel_cons
            'Sprat
            (__toplevel_cons
               'could
               (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ()))))))))
   (<change>
      (lat?
         (__toplevel_cons
            (__toplevel_cons 'Jack ())
            (__toplevel_cons
               'Sprat
               (__toplevel_cons
                  'could
                  (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ()))))))))
      ())
   (<change>
      (lat?
         (__toplevel_cons
            'Jack
            (__toplevel_cons
               (__toplevel_cons 'Sprat (__toplevel_cons 'could ()))
               (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ())))))))
      ())
   (lat? ()))