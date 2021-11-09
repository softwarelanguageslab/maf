; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((phi (lambda (x1 x2 x3 x4)
                (if (let ((__or_res x1)) (if __or_res __or_res (let ((__or_res (not x2))) (if __or_res __or_res (not x3)))))
                   (if (let ((__or_res (not x2))) (if __or_res __or_res (not x3)))
                      (let ((__or_res x4))
                         (<change>
                            ()
                            x2)
                         (if __or_res __or_res x2))
                      #f)
                   #f)))
         (try (lambda (f)
                (let ((__or_res (f #t)))
                   (if __or_res
                      (<change>
                         __or_res
                         (f #f))
                      (<change>
                         (f #f)
                         __or_res)))))
         (sat-solve-4 (lambda (p)
                        (try (lambda (n1) (try (lambda (n2) (try (lambda (n3) (try (lambda (n4) (p n1 n2 n3 n4))))))))))))
   (sat-solve-4 phi))