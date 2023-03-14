; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((calc-e-iter (lambda (n)
                        (letrec ((iter (lambda (ctr res fac-prev)
                                         (if (> ctr n)
                                            res
                                            (let ((new-fac (* ctr fac-prev)))
                                               (iter (+ ctr 1) (+ res (/ 1 new-fac)) new-fac))))))
                           (<change>
                              ()
                              1)
                           (iter 1 1 1))))
         (calc-cos (lambda (x n)
                     (letrec ((iter (lambda (ctr acc fac xpow sign)
                                      (if (>= ctr n)
                                         acc
                                         (let* ((i (* 2 ctr))
                                                (newfac (* fac (- i 1) i))
                                                (newxpow (* xpow x x))
                                                (newsign (- sign)))
                                            (iter (+ ctr 1) (+ acc (/ (* newsign newxpow) newfac)) newfac newxpow newsign))))))
                        (iter 1 1 1 1 1))))
         (close-to (lambda (x y)
                     (< (abs (- x y)) 1.000000e-08))))
   (if (close-to (exact->inexact (calc-e-iter 10)) 2.718282e+00)
      (if (close-to (calc-cos 0 10) 1)
         (if (close-to (calc-cos (/ 3.141500e+00 2) 10) 4.632679e-05)
            (close-to (calc-cos 3.141500e+00 10) -1.000000e+00)
            #f)
         #f)
      #f))