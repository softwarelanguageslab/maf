; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((div2* (lambda (n s)
                  (if (= (* 2 n) s)
                     n
                     (if (= (+ (* 2 n) 1) s) n (div2* (- n 1) s)))))
         (div2 (lambda (n)
                 (div2* n n)))
         (hailstone* (lambda (n count)
                       (if (= n 1)
                          count
                          (if (<change> (even? n) (not (even? n)))
                             (hailstone* (div2 n) (+ count 1))
                             (hailstone* (+ (* 3 n) 1) (+ count 1))))))
         (hailstone (lambda (n)
                      (<change>
                         ()
                         hailstone*)
                      (hailstone* n 0))))
   (hailstone 5))