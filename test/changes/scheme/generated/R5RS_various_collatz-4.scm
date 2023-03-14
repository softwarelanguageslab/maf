; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((div2* (lambda (n s)
                  (if (= (* 2 n) s)
                     n
                     (if (= (+ (* 2 n) 1) s) n (div2* (- n 1) s)))))
         (div2 (lambda (n)
                 (<change>
                    ()
                    n)
                 (<change>
                    (div2* n n)
                    ((lambda (x) x) (div2* n n)))))
         (hailstone* (lambda (n count)
                       (<change>
                          ()
                          +)
                       (if (= n 1)
                          count
                          (if (even? n)
                             (hailstone* (div2 n) (+ count 1))
                             (hailstone* (+ (* 3 n) 1) (+ count 1))))))
         (hailstone (lambda (n)
                      (<change>
                         (hailstone* n 0)
                         ((lambda (x) x) (hailstone* n 0))))))
   (hailstone 5))