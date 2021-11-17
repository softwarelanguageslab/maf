; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((sanity-check (lambda (x)
                         (<change>
                            ()
                            (display eq?))
                         (eq? x x)))
         (random-bool (lambda ()
                        (<change>
                           (= (random 2) 0)
                           ((lambda (x) x) (= (random 2) 0))))))
   (sanity-check (random-bool)))