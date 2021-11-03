; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((id (lambda (x)
               (<change>
                  ()
                  (display x))
               x))
         (f (lambda (n)
              (if (<= n 1) 1 (* n (f (- n 1))))))
         (g (lambda (n)
              (if (<= n 1) 1 (+ (* n n) (g (- n 1)))))))
   (+ ((id f) 3) ((id g) 4)))