; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 0
(letrec ((atom? (lambda (x)
                  (if (not (pair? x)) (not (null? x)) #f))))
   (atom? 'atom)
   (<change>
      (atom? 'turkey)
      ())
   (<change>
      (atom? 1942)
      (atom? 'u))
   (<change>
      (atom? 'u)
      (atom? 1942))
   (atom? '*abc$)
   (list? (__toplevel_cons 'atom ()))
   (list? (__toplevel_cons 'atom (__toplevel_cons 'turkey (__toplevel_cons 'or ()))))
   (list?
      (__toplevel_cons (__toplevel_cons 'atom (__toplevel_cons 'turkey ())) (__toplevel_cons 'or ())))
   (list? ())
   (atom? ())
   (car (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
   (car
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
         (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
   (cdr (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
   (cdr
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
         (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))
   (cons 'peanut (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ()))))
   (cons
      (__toplevel_cons 'banana (__toplevel_cons 'and ()))
      (__toplevel_cons
         'peanut
         (__toplevel_cons 'butter (__toplevel_cons 'and (__toplevel_cons 'jelly ())))))
   (null? ()))