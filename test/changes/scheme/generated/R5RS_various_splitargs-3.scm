; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((sanity-check (lambda (x)
                         (eq? x x)))
         (random-bool (lambda ()
                        (<change>
                           ()
                           (display (= (random 2) 0)))
                        (<change>
                           ()
                           (display 0))
                        (= (random 2) 0))))
   (sanity-check (random-bool)))