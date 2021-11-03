; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((id (lambda (x)
               x))
         (f (lambda (n)
              (<change>
                 ()
                 n)
              (if (<= n 1) 1 (* n (f (- n 1))))))
         (g (lambda (n)
              (if (<= n 1) 1 (+ (* n n) (g (- n 1)))))))
   (<change>
      ()
      (id f))
   (+ ((id f) 3) ((id g) 4)))