; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((sanity-check (lambda (x)
                         (<change>
                            ()
                            (eq? x x))
                         (eq? x x)))
         (random-bool (lambda ()
                        (= (random 2) 0))))
   (sanity-check (random-bool)))