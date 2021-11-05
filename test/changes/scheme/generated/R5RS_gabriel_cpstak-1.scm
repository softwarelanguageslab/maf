; Changes:
; * removed: 2
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((tak (lambda (x y z k)
                (<change>
                   @sensitivity:FA
                   ())
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
                   ((lambda (x) x)
                      (if (not (< y x))
                         (<change>
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
                                        (display tak)
                                        @sensitivity:FA
                                        (tak (- z 1) x y (lambda (v3) @sensitivity:FA (tak v1 v2 v3 k))))))))
                         (<change>
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
                                        (tak (- z 1) x y (lambda (v3) @sensitivity:FA (tak v1 v2 v3 k)))))))
                            (k z)))))))
         (res (tak 20 10 5 (lambda (a) (<change> @sensitivity:FA ()) a))))
   res)