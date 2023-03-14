; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((id (lambda (x)
               x))
         (f (lambda (n)
              (if (<= n 1) 1 (* n (f (- n 1))))))
         (g (lambda (n)
              (if (<= n 1) 1 (+ (* n n) (g (- n 1)))))))
   (<change>
      ()
      4)
   (+ ((id f) 3) ((id g) 4)))