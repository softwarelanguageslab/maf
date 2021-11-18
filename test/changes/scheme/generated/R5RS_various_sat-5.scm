; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((phi (lambda (x1 x2 x3 x4)
                (if (let ((__or_res x1)) (if __or_res __or_res (let ((__or_res (not x2))) (if __or_res __or_res (not x3)))))
                   (if (<change> (let ((__or_res (not x2))) (if __or_res __or_res (not x3))) (not (let ((__or_res (not x2))) (if __or_res __or_res (not x3)))))
                      (let ((__or_res x4))
                         (if __or_res
                            (<change>
                               __or_res
                               x2)
                            (<change>
                               x2
                               __or_res)))
                      #f)
                   #f)))
         (try (lambda (f)
                (let ((__or_res (f #t)))
                   (if __or_res __or_res (f #f)))))
         (sat-solve-4 (lambda (p)
                        (try (lambda (n1) (try (lambda (n2) (try (lambda (n3) (try (lambda (n4) (p n1 n2 n3 n4))))))))))))
   (sat-solve-4 phi))