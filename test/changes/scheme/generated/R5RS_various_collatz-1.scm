; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((div2* (lambda (n s)
                  (if (= (* 2 n) s)
                     n
                     (if (<change> (= (+ (* 2 n) 1) s) (not (= (+ (* 2 n) 1) s)))
                        n
                        (div2* (- n 1) s)))))
         (div2 (lambda (n)
                 (<change>
                    (div2* n n)
                    ((lambda (x) x) (div2* n n)))))
         (hailstone* (lambda (n count)
                       (if (= n 1)
                          count
                          (if (even? n)
                             (hailstone* (div2 n) (+ count 1))
                             (hailstone* (+ (* 3 n) 1) (+ count 1))))))
         (hailstone (lambda (n)
                      (hailstone* n 0))))
   (hailstone 5))