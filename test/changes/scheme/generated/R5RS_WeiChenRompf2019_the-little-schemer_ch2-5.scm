; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((atom? (lambda (x)
                  (if (not (pair? x)) (not (null? x)) #f)))
         (lat? (lambda (l)
                 (<change>
                    ()
                    atom?)
                 (<change>
                    (if (null? l)
                       #t
                       (if (atom? (car l)) (lat? (cdr l)) #f))
                    ((lambda (x) x) (if (null? l) #t (if (atom? (car l)) (lat? (cdr l)) #f)))))))
   (<change>
      (lat?
         (__toplevel_cons
            'Jack
            (__toplevel_cons
               'Sprat
               (__toplevel_cons
                  'could
                  (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ()))))))))
      (lat?
         (__toplevel_cons
            (__toplevel_cons 'Jack ())
            (__toplevel_cons
               'Sprat
               (__toplevel_cons
                  'could
                  (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ())))))))))
   (<change>
      (lat?
         (__toplevel_cons
            (__toplevel_cons 'Jack ())
            (__toplevel_cons
               'Sprat
               (__toplevel_cons
                  'could
                  (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ()))))))))
      (lat?
         (__toplevel_cons
            'Jack
            (__toplevel_cons
               'Sprat
               (__toplevel_cons
                  'could
                  (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ())))))))))
   (lat?
      (__toplevel_cons
         'Jack
         (__toplevel_cons
            (__toplevel_cons 'Sprat (__toplevel_cons 'could ()))
            (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ())))))))
   (<change>
      (lat? ())
      ((lambda (x) x) (lat? ()))))