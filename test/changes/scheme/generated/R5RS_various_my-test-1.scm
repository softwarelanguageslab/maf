; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((random-bool (lambda ()
                        (= (random 2) 0)))
         (f (lambda (x)
              (if (<change> (random-bool) (not (random-bool)))
                 x
                 (g (cons 'f x)))))
         (g (lambda (x)
              (if (<change> (random-bool) (not (random-bool)))
                 x
                 (f (cons 'g x))))))
   (f ()))