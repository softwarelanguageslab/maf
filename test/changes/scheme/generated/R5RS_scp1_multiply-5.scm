; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((rec-multiply (lambda (a b)
                         (<change>
                            (if (zero? b) 0 (+ a (rec-multiply a (- b 1))))
                            ((lambda (x) x) (if (<change> (zero? b) (not (zero? b))) 0 (+ a (rec-multiply a (- b 1))))))))
         (iter-multiply (lambda (a b)
                          (letrec ((iter (lambda (result counter)
                                           (if (<change> (zero? counter) (not (zero? counter)))
                                              result
                                              (iter (+ result a) (- counter 1))))))
                             (<change>
                                (iter 0 b)
                                ((lambda (x) x) (iter 0 b)))))))
   (= 10 (rec-multiply 5 2) (iter-multiply 5 2)))