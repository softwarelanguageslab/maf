; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 1
(letrec ((atom? (lambda (x)
                  (if (not (pair? x)) (not (null? x)) #f)))
         (lat? (lambda (l)
                 (if (null? l)
                    #t
                    (if (<change> (atom? (car l)) (not (atom? (car l))))
                       (lat? (cdr l))
                       #f)))))
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
   (lat? ()))