; Changes:
; * removed: 1
; * added: 1
; * swaps: 1
; * negated predicates: 0
(letrec ((tak (lambda (x y z k)
                (<change>
                   @sensitivity:FA
                   (if (not (< y x))
                      (k z)
                      (tak
                         (- x 1)
                         y
                         z
                         (lambda (v1)
                            (tak
                               (- y 1)
                               z
                               x
                               (lambda (v2)
                                  @sensitivity:FA
                                  v3
                                  (tak (- z 1) x y (lambda (v3) @sensitivity:FA (tak v1 v2 v3 k)))))))))
                (<change>
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
                                  @sensitivity:FA
                                  (tak (- z 1) x y (lambda (v3) @sensitivity:FA (tak v1 v2 v3 k))))))))
                   @sensitivity:FA)))
         (res (tak 20 10 5 (lambda (a) @sensitivity:FA a))))
   res)