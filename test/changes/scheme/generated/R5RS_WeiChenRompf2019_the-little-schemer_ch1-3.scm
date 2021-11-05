; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 3
(letrec ((atom? (lambda (x)
                  (if (not (pair? x))
                     (<change>
                        (not (null? x))
                        #f)
                     (<change>
                        #f
                        (not (null? x)))))))
   (<change>
      (atom? 'atom)
      ((lambda (x) x) (atom? 'atom)))
   (atom? 'turkey)
   (atom? 1942)
   (atom? 'u)
   (atom? '*abc$)
   (list? (__toplevel_cons 'atom ()))
   (list? (__toplevel_cons 'atom (__toplevel_cons 'turkey (__toplevel_cons 'or ()))))
   (list?
      (__toplevel_cons (__toplevel_cons 'atom (__toplevel_cons 'turkey ())) (__toplevel_cons 'or ())))
   (<change>
      (list? ())
      ((lambda (x) x) (list? ())))
   (atom? ())
   (<change>
      (car (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
      (car
         (__toplevel_cons
            (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
            (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))
   (<change>
      (car
         (__toplevel_cons
            (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
            (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
      (car (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))))
   (cdr (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
   (cdr
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
         (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
   (cons 'peanut (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ()))))
   (<change>
      (cons
         (__toplevel_cons 'banana (__toplevel_cons 'and ()))
         (__toplevel_cons
            'peanut
            (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ())))))
      ((lambda (x) x)
         (cons
            (__toplevel_cons 'banana (__toplevel_cons 'and ()))
            (__toplevel_cons
               'peanut
               (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ())))))))
   (null? ()))