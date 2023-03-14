; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
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
                                                      (iter (double a) (halve b) acc)
                                                      (iter a (- b 1) (+ acc a)))))))
                                  (iter a b 0)))))
   (<change>
      ()
      iter-fast-multiply)
   (<change>
      (if (= (rec-fast-multiply 3 4) 12)
         (if (= (rec-fast-multiply 100 200) 20000)
            (if (= (iter-fast-multiply 3 4) 12)
               (= (iter-fast-multiply 100 200) 20000)
               #f)
            #f)
         #f)
      ((lambda (x) x)
         (if (= (rec-fast-multiply 3 4) 12)
            (if (= (rec-fast-multiply 100 200) 20000)
               (<change>
                  (if (= (iter-fast-multiply 3 4) 12)
                     (= (iter-fast-multiply 100 200) 20000)
                     #f)
                  #f)
               (<change>
                  #f
                  (if (= (iter-fast-multiply 3 4) 12)
                     (= (iter-fast-multiply 100 200) 20000)
                     #f)))
            #f))))