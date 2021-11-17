; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((double (lambda (x)
                   (+ x x)))
         (halve (lambda (x)
                  (<change>
                     ()
                     (display 2))
                  (/ x 2)))
         (sim-multiply (lambda (a b)
                         (<change>
                            (if (zero? b) 1 (+ 1 (sim-multiply a (- b 1))))
                            ((lambda (x) x) (if (zero? b) 1 (+ 1 (sim-multiply a (- b 1))))))))
         (sim-fast-multiply (lambda (a b)
                              (if (zero? b)
                                 1
                                 (if (even? b)
                                    (+ 1 (sim-fast-multiply (double a) (halve b)))
                                    (+ 1 (sim-fast-multiply a (- b 1))))))))
   (if (= (sim-multiply 14 2365) 2366)
      (= (sim-fast-multiply 14 2365) 19)
      #f))