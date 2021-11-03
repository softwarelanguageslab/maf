; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 0
(letrec ((tak (lambda (x y z k)
                @sensitivity:FA
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
                                  (tak (- z 1) x y (lambda (v3) v3 @sensitivity:FA (tak v1 v2 v3 k))))
                               (<change>
                                  (tak (- z 1) x y (lambda (v3) @sensitivity:FA (tak v1 v2 v3 k)))
                                  @sensitivity:FA))))))))
         (res (tak 20 10 5 (lambda (a) (<change> () @sensitivity:FA) @sensitivity:FA a))))
   res)