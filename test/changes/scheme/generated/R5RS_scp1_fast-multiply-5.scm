; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 3
; * calls to id fun: 0
(letrec ((double (lambda (x)
                   (+ x x)))
         (halve (lambda (x)
                  (/ x 2)))
         (rec-fast-multiply (lambda (a b)
                              (if (zero? b)
                                 0
                                 (if (even? b)
                                    (rec-fast-multiply (double a) (halve b))
                                    (+ a (rec-fast-multiply a (- b 1)))))))
         (iter-fast-multiply (lambda (a b)
                               (letrec ((iter (lambda (a b acc)
                                                (if (zero? b)
                                                   acc
                                                   (if (even? b)
                                                      (<change>
                                                         (iter (double a) (halve b) acc)
                                                         (iter a (- b 1) (+ acc a)))
                                                      (<change>
                                                         (iter a (- b 1) (+ acc a))
                                                         (iter (double a) (halve b) acc)))))))
                                  (<change>
                                     ()
                                     0)
                                  (iter a b 0)))))
   (if (= (rec-fast-multiply 3 4) 12)
      (<change>
         (if (= (rec-fast-multiply 100 200) 20000)
            (if (= (iter-fast-multiply 3 4) 12)
               (= (iter-fast-multiply 100 200) 20000)
               #f)
            #f)
         #f)
      (<change>
         #f
         (if (= (rec-fast-multiply 100 200) 20000)
            (if (= (iter-fast-multiply 3 4) 12)
               #f
               (= (iter-fast-multiply 100 200) 20000))
            #f))))