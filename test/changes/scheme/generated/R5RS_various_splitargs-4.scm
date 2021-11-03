; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((sanity-check (lambda (x)
                         (eq? x x)))
         (random-bool (lambda ()
                        (= (random 2) 0))))
   (<change>
      ()
      sanity-check)
   (sanity-check (random-bool)))