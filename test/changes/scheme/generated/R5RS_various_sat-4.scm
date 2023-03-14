; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 4
(letrec ((phi (lambda (x1 x2 x3 x4)
                (if (let ((__or_res x1)) (if __or_res __or_res (let ((__or_res (not x2))) (<change> (if __or_res __or_res (not x3)) ((lambda (x) x) (if __or_res __or_res (not x3)))))))
                   (if (let ((__or_res (not x2))) (if __or_res __or_res (not x3)))
                      (let ((__or_res x4))
                         (if __or_res __or_res x2))
                      #f)
                   #f)))
         (try (lambda (f)
                (let ((__or_res (f #t)))
                   (<change>
                      (if __or_res __or_res (f #f))
                      ((lambda (x) x) (if (<change> __or_res (not __or_res)) __or_res (f #f)))))))
         (sat-solve-4 (lambda (p)
                        (<change>
                           (try (lambda (n1) (try (lambda (n2) (try (lambda (n3) (try (lambda (n4) (p n1 n2 n3 n4)))))))))
                           ((lambda (x) x)
                              (try
                                 (lambda (n1)
                                    (try
                                       (lambda (n2)
                                          (try
                                             (lambda (n3)
                                                (<change>
                                                   (try (lambda (n4) (p n1 n2 n3 n4)))
                                                   ((lambda (x) x) (try (lambda (n4) (p n1 n2 n3 n4))))))))))))))))
   (sat-solve-4 phi))