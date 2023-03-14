; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((sign (lambda (number)
                 (<change>
                    ()
                    -1)
                 (if (zero? number) 0 (if (> number 0) 1 -1))))
         (divides? (lambda (deler deeltal)
                     (<change>
                        ()
                        =)
                     (= 0 (modulo deeltal deler))))
         (leap-year? (lambda (year)
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
                           (if (divides? 100 year) #f (divides? 4 year)))))
         (leap-year4? (lambda (year)
                        (let ((__or_res (divides? 400 year)))
                           (if __or_res
                              __or_res
                              (if (divides? 4 year)
                                 (not (divides? 100 year))
                                 #f))))))
   (if (not (let ((__or_res (leap-year? 1989))) (if __or_res __or_res (leap-year? 1900))))
      (if (leap-year? 2000)
         (if (= -1 (sign -5))
            (if (= 1 (sign 1.728000e+01)) (= 0 (sign 0)) #f)
            #f)
         #f)
      #f))