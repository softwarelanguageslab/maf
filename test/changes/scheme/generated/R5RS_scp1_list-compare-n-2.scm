; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 2
; * calls to id fun: 1
(letrec ((compare (lambda (lijst1 lijst2)
                    (if (let ((__or_res (null? lijst1))) (if __or_res __or_res (null? lijst2)))
                       0
                       (if (eq? (car lijst1) (car lijst2))
                          (+ 1 (compare (cdr lijst1) (cdr lijst2)))
                          0))))
         (compare-iter (lambda (lijst1 lijst2)
                         (<change>
                            (letrec ((loop (lambda (l1 l2 res)
                                             (if (let ((__or_res (null? l1))) (if __or_res __or_res (null? l2)))
                                                res
                                                (if (eq? (car l1) (car l2))
                                                   (loop (cdr l1) (cdr l2) (+ res 1))
                                                   res)))))
                               (loop lijst1 lijst2 0))
                            ((lambda (x) x)
                               (letrec ((loop (lambda (l1 l2 res)
                                                (if (let ((__or_res (null? l1))) (if (<change> __or_res (not __or_res)) __or_res (null? l2)))
                                                   res
                                                   (if (<change> (eq? (car l1) (car l2)) (not (eq? (car l1) (car l2))))
                                                      (loop (cdr l1) (cdr l2) (+ res 1))
                                                      res)))))
                                  (loop lijst1 lijst2 0))))))
         (algemene-compare (lambda (lijst1 lijst2 test)
                             (if (let ((__or_res (null? lijst1))) (if __or_res (<change> __or_res (null? lijst2)) (<change> (null? lijst2) __or_res)))
                                0
                                (if (test (car lijst1) (car lijst2))
                                   (+ 1 (algemene-compare (cdr lijst1) (cdr lijst2) test))
                                   0))))
         (compare-greater (lambda (lijst1 lijst2)
                            (algemene-compare lijst1 lijst2 >))))
   (if (= (compare (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ()))))))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'x (__toplevel_cons 'y ())))))) 3)
      (if (= (compare (__toplevel_cons 'x (__toplevel_cons 'a (__toplevel_cons 'b ()))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))))) 0)
         (if (= (compare (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))) (__toplevel_cons 'a (__toplevel_cons 'b ()))) 2)
            (<change>
               (if (= (compare-iter (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ()))))))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'x (__toplevel_cons 'y ())))))) 3)
                  (if (= (compare-iter (__toplevel_cons 'x (__toplevel_cons 'a (__toplevel_cons 'b ()))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))))) 0)
                     (if (= (compare-iter (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))) (__toplevel_cons 'a (__toplevel_cons 'b ()))) 2)
                        (=
                           (compare-greater
                              (__toplevel_cons
                                 3
                                 (__toplevel_cons
                                    5
                                    (__toplevel_cons 6 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 5 ()))))))
                              (__toplevel_cons
                                 2
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons 0 (__toplevel_cons 8 (__toplevel_cons 5 (__toplevel_cons 5 ())))))))
                           3)
                        #f)
                     #f)
                  #f)
               #f)
            (<change>
               #f
               (if (= (compare-iter (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ()))))))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'x (__toplevel_cons 'y ())))))) 3)
                  (if (= (compare-iter (__toplevel_cons 'x (__toplevel_cons 'a (__toplevel_cons 'b ()))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))))) 0)
                     (if (= (compare-iter (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))) (__toplevel_cons 'a (__toplevel_cons 'b ()))) 2)
                        (=
                           (compare-greater
                              (__toplevel_cons
                                 3
                                 (__toplevel_cons
                                    5
                                    (__toplevel_cons 6 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 5 ()))))))
                              (__toplevel_cons
                                 2
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons 0 (__toplevel_cons 8 (__toplevel_cons 5 (__toplevel_cons 5 ())))))))
                           3)
                        #f)
                     #f)
                  #f)))
         #f)
      #f))