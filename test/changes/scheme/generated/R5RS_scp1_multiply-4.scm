; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 2
(letrec ((rec-multiply (lambda (a b)
                         (<change>
                            (if (zero? b) 0 (+ a (rec-multiply a (- b 1))))
                            ((lambda (x) x)
                               (if (zero? b)
                                  (<change>
                                     0
                                     (+ a (rec-multiply a (- b 1))))
                                  (<change>
                                     (+ a (rec-multiply a (- b 1)))
                                     0))))))
         (iter-multiply (lambda (a b)
                          (<change>
                             (letrec ((iter (lambda (result counter)
                                              (if (zero? counter)
                                                 result
                                                 (iter (+ result a) (- counter 1))))))
                                (iter 0 b))
                             ((lambda (x) x)
                                (letrec ((iter (lambda (result counter)
                                                 (<change>
                                                    ()
                                                    (display -))
                                                 (if (zero? counter)
                                                    result
                                                    (iter (+ result a) (- counter 1))))))
                                   (iter 0 b)))))))
   (= 10 (rec-multiply 5 2) (iter-multiply 5 2)))