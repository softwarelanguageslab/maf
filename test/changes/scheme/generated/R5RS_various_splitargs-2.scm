; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((sanity-check (lambda (x)
                         (eq? x x)))
         (random-bool (lambda ()
                        (<change>
                           ()
                           2)
                        (= (random 2) 0))))
   (<change>
      (sanity-check (random-bool))
      ((lambda (x) x) (sanity-check (random-bool)))))