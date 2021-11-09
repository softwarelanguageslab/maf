; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((random-bool (lambda ()
                        (= (random 2) 0)))
         (f (lambda (x)
              (if (random-bool) x (g (cons 'f x)))))
         (g (lambda (x)
              (<change>
                 (if (random-bool) x (f (cons 'g x)))
                 ((lambda (x) x) (if (random-bool) x (f (cons 'g x))))))))
   (<change>
      ()
      (f ()))
   (f ()))