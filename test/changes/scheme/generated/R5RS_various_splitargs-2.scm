; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((sanity-check (lambda (x)
                         (eq? x x)))
         (random-bool (lambda ()
                        (= (random 2) 0))))
   (<change>
      ()
      (display (sanity-check (random-bool))))
   (sanity-check (random-bool)))