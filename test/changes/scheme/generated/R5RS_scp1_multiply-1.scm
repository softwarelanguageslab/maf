; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((rec-multiply (lambda (a b)
                         (if (<change> (zero? b) (not (zero? b)))
                            0
                            (+ a (rec-multiply a (- b 1))))))
         (iter-multiply (lambda (a b)
                          (<change>
                             ()
                             zero?)
                          (letrec ((iter (lambda (result counter)
                                           (<change>
                                              ()
                                              zero?)
                                           (if (zero? counter)
                                              result
                                              (iter (+ result a) (- counter 1))))))
                             (iter 0 b)))))
   (= 10 (rec-multiply 5 2) (iter-multiply 5 2)))