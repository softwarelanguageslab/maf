; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((random-bool (lambda ()
                        (= (random 2) 0)))
         (f (lambda (x)
              (if (<change> (random-bool) (not (random-bool)))
                 x
                 (g (cons 'f x)))))
         (g (lambda (x)
              (if (random-bool) x (f (cons 'g x))))))
   (<change>
      (f ())
      ((lambda (x) x) (f ()))))