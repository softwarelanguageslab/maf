; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((double (lambda (x)
                   (<change>
                      (+ x x)
                      ((lambda (x) x) (+ x x)))))
         (halve (lambda (x)
                  (/ x 2)))
         (sim-multiply (lambda (a b)
                         (if (zero? b) 1 (+ 1 (sim-multiply a (- b 1))))))
         (sim-fast-multiply (lambda (a b)
                              (if (zero? b)
                                 1
                                 (if (even? b)
                                    (+ 1 (sim-fast-multiply (double a) (halve b)))
                                    (+ 1 (sim-fast-multiply a (- b 1))))))))
   (if (<change> (= (sim-multiply 14 2365) 2366) (not (= (sim-multiply 14 2365) 2366)))
      (= (sim-fast-multiply 14 2365) 19)
      #f))