; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((sanity-check (lambda (x)
                         (<change>
                            ()
                            x)
                         (eq? x x)))
         (random-bool (lambda ()
                        (= (random 2) 0))))
   (sanity-check (random-bool)))