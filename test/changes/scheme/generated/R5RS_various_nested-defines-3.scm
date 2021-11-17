; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((f (lambda (x)
              (letrec ((g (lambda (y)
                            (<change>
                               ()
                               (display y))
                            (+ x y))))
                 (g 5)))))
   (= (f 0) 5))