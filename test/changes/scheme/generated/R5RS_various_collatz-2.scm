; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((div2* (lambda (n s)
                  (if (= (* 2 n) s)
                     n
                     (if (= (+ (* 2 n) 1) s)
                        (<change>
                           n
                           (div2* (- n 1) s))
                        (<change>
                           (div2* (- n 1) s)
                           n)))))
         (div2 (lambda (n)
                 (<change>
                    ()
                    div2*)
                 (div2* n n)))
         (hailstone* (lambda (n count)
                       (<change>
                          (if (= n 1)
                             count
                             (if (even? n)
                                (hailstone* (div2 n) (+ count 1))
                                (hailstone* (+ (* 3 n) 1) (+ count 1))))
                          ((lambda (x) x)
                             (if (= n 1)
                                count
                                (if (even? n)
                                   (hailstone* (div2 n) (+ count 1))
                                   (hailstone* (+ (* 3 n) 1) (+ count 1))))))))
         (hailstone (lambda (n)
                      (hailstone* n 0))))
   (hailstone 5))