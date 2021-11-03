; Changes:
; * removed: 0
; * added: 1
; * swaps: 3
; * negated predicates: 0
(letrec ((atom? (lambda (x)
                  (<change>
                     ()
                     null?)
                  (if (not (pair? x)) (not (null? x)) #f))))
   (<change>
      (atom? 'atom)
      (atom? 'turkey))
   (<change>
      (atom? 'turkey)
      (atom? 'atom))
   (atom? 1942)
   (<change>
      (atom? 'u)
      (atom? '*abc$))
   (<change>
      (atom? '*abc$)
      (atom? 'u))
   (<change>
      (list? (__toplevel_cons 'atom ()))
      (list? (__toplevel_cons 'atom (__toplevel_cons 'turkey (__toplevel_cons 'or ())))))
   (<change>
      (list? (__toplevel_cons 'atom (__toplevel_cons 'turkey (__toplevel_cons 'or ()))))
      (list? (__toplevel_cons 'atom ())))
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