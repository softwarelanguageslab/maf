; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((double (lambda (x)
                   (<change>
                      ()
                      x)
                   (<change>
                      ()
                      x)
                   (+ x x)))
         (halve (lambda (x)
                  (<change>
                     (/ x 2)
                     ((lambda (x) x) (/ x 2)))))
         (sim-multiply (lambda (a b)
                         (if (zero? b) 1 (+ 1 (sim-multiply a (- b 1))))))
         (sim-fast-multiply (lambda (a b)
                              (if (zero? b)
                                 1
                                 (if (even? b)
                                    (+ 1 (sim-fast-multiply (double a) (halve b)))
                                    (+ 1 (sim-fast-multiply a (- b 1))))))))
   (if (= (sim-multiply 14 2365) 2366)
      (<change>
         (= (sim-fast-multiply 14 2365) 19)
         #f)
      (<change>
         #f
         (= (sim-fast-multiply 14 2365) 19))))