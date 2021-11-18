; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 3
; * calls to id fun: 1
(letrec ((sign (lambda (number)
                 (if (zero? number)
                    (<change>
                       0
                       (if (> number 0) 1 -1))
                    (<change>
                       (if (> number 0) 1 -1)
                       0))))
         (divides? (lambda (deler deeltal)
                     (<change>
                        ()
                        deeltal)
                     (= 0 (modulo deeltal deler))))
         (leap-year? (lambda (year)
                       (<change>
                          (if (divides? 4 year)
                             (if (divides? 100 year) (divides? 400 year) #t)
                             #f)
                          ((lambda (x) x)
                             (if (divides? 4 year)
                                (<change>
                                   (if (divides? 100 year) (divides? 400 year) #t)
                                   #f)
                                (<change>
                                   #f
                                   (if (divides? 100 year) (divides? 400 year) #t)))))))
         (leap-year2? (lambda (year)
                        (if (divides? 400 year)
                           #t
                           (if (divides? 100 year)
                              (<change>
                                 #f
                                 (if (divides? 4 year) #t #f))
                              (<change>
                                 (if (divides? 4 year) #t #f)
                                 #f)))))
         (leap-year3? (lambda (year)
                        (if (divides? 400 year)
                           #t
                           (if (divides? 100 year) #f (divides? 4 year)))))
         (leap-year4? (lambda (year)
                        (let ((__or_res (divides? 400 year)))
                           (if __or_res
                              __or_res
                              (if (divides? 4 year)
                                 (not (divides? 100 year))
                                 #f))))))
   (<change>
      ()
      sign)
   (if (not (let ((__or_res (leap-year? 1989))) (<change> () (if __or_res __or_res (leap-year? 1900))) (if (<change> __or_res (not __or_res)) __or_res (leap-year? 1900))))
      (if (leap-year? 2000)
         (if (<change> (= -1 (sign -5)) (not (= -1 (sign -5))))
            (if (= 1 (sign 1.728000e+01)) (= 0 (sign 0)) #f)
            #f)
         #f)
      #f))