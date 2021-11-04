; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((tak (lambda (x y z k)
                (<change>
                   @sensitivity:FA
                   ((lambda (x) x) @sensitivity:FA))
                (<change>
                   ()
                   @sensitivity:FA)
                (if (not (< y x))
                   (k z)
                   (tak
                      (- x 1)
                      y
                      z
                      (lambda (v1)
                         @sensitivity:FA
                         (tak
                            (- y 1)
                            z
                            x
                            (lambda (v2)
                               (<change>
                                  @sensitivity:FA
                                  ((lambda (x) x) @sensitivity:FA))
                               (tak (- z 1) x y (lambda (v3) @sensitivity:FA (tak v1 v2 v3 k))))))))))
         (res (tak 20 10 5 (lambda (a) @sensitivity:FA a))))
   res)