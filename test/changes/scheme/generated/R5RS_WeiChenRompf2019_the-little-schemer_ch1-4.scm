; Changes:
; * removed: 1
; * added: 4
; * swaps: 3
; * negated predicates: 0
(letrec ((atom? (lambda (x)
                  (if (not (pair? x)) (not (null? x)) #f))))
   (atom? 'atom)
   (atom? 'turkey)
   (atom? 1942)
   (atom? 'u)
   (<change>
      ()
      (__toplevel_cons 'banana (__toplevel_cons 'and ())))
   (<change>
      (atom? '*abc$)
      (list? (__toplevel_cons 'atom ())))
   (<change>
      (list? (__toplevel_cons 'atom ()))
      (atom? '*abc$))
   (list? (__toplevel_cons 'atom (__toplevel_cons 'turkey (__toplevel_cons 'or ()))))
   (<change>
      (list?
         (__toplevel_cons (__toplevel_cons 'atom (__toplevel_cons 'turkey ())) (__toplevel_cons 'or ())))
      (list? ()))
   (<change>
      (list? ())
      (list?
         (__toplevel_cons (__toplevel_cons 'atom (__toplevel_cons 'turkey ())) (__toplevel_cons 'or ()))))
   (atom? ())
   (<change>
      ()
      __toplevel_cons)
   (<change>
      (car (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
      ())
   (car
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
         (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
   (<change>
      ()
      (atom? 'u))
   (cdr (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
   (<change>
      ()
      __toplevel_cons)
   (cdr
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
         (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
   (<change>
      (cons 'peanut (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ()))))
      (cons
         (__toplevel_cons 'banana (__toplevel_cons 'and ()))
         (__toplevel_cons
            'peanut
            (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ()))))))
   (<change>
      (cons
         (__toplevel_cons 'banana (__toplevel_cons 'and ()))
         (__toplevel_cons
            'peanut
            (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ())))))
      (cons 'peanut (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ())))))
   (null? ()))