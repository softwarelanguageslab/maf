; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 2
; * calls to id fun: 0
(letrec ((sign (lambda (number)
                 (if (zero? number) 0 (if (> number 0) 1 -1))))
         (divides? (lambda (deler deeltal)
                     (= 0 (modulo deeltal deler))))
         (leap-year? (lambda (year)
                       (<change>
                          ()
                          (divides? 4 year))
                       (if (divides? 4 year)
                          (if (divides? 100 year) (divides? 400 year) #t)
                          #f)))
         (leap-year2? (lambda (year)
                        (if (divides? 400 year)
                           #t
                           (if (divides? 100 year)
                              #f
                              (if (divides? 4 year) #t #f)))))
         (leap-year3? (lambda (year)
                        (if (divides? 400 year)
                           #t
                           (if (divides? 100 year)
                              (<change>
                                 #f
                                 (divides? 4 year))
                              (<change>
                                 (divides? 4 year)
                                 #f)))))
         (leap-year4? (lambda (year)
                        (let ((__or_res (divides? 400 year)))
                           (if (<change> __or_res (not __or_res))
                              __or_res
                              (if (divides? 4 year)
                                 (<change>
                                    (not (divides? 100 year))
                                    #f)
                                 (<change>
                                    #f
                                    (not (divides? 100 year)))))))))
   (if (not (let ((__or_res (leap-year? 1989))) (if __or_res __or_res (leap-year? 1900))))
      (if (leap-year? 2000)
         (if (= -1 (sign -5))
            (if (= 1 (sign 1.728000e+01)) (= 0 (sign 0)) #f)
            #f)
         #f)
      #f))