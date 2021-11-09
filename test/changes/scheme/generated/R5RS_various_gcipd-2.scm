; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((id (lambda (x)
               (<change>
                  ()
                  x)
               x))
         (f (lambda (n)
              (if (<= n 1) 1 (* n (f (- n 1))))))
         (g (lambda (n)
              (<change>
                 (if (<= n 1) 1 (+ (* n n) (g (- n 1))))
                 ((lambda (x) x) (if (<= n 1) 1 (+ (* n n) (g (- n 1)))))))))
   (+ ((id f) 3) ((id g) 4)))