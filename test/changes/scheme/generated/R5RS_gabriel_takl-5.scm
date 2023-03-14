; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((listn (lambda (n)
                  @sensitivity:FA
                  (<change>
                     (if (= n 0) () (cons n (listn (- n 1))))
                     ((lambda (x) x) (if (= n 0) () (cons n (listn (- n 1))))))))
         (shorterp (lambda (x y)
                     (<change>
                        @sensitivity:FA
                        (if (not (not (null? y)))
                           (let ((__or_res (null? x)))
                              ((lambda (x) x) (if __or_res __or_res (shorterp (cdr x) (cdr y)))))
                           #f))
                     (<change>
                        (if (not (null? y))
                           (let ((__or_res (null? x)))
                              (if __or_res __or_res (shorterp (cdr x) (cdr y))))
                           #f)
                        @sensitivity:FA)))
         (mas (lambda (x y z)
                @sensitivity:FA
                (if (not (shorterp y x))
                   z
                   (mas (mas (cdr x) y z) (mas (cdr y) z x) (mas (cdr z) x y))))))
   (let ((result (__toplevel_cons
                   7
                   (__toplevel_cons
                      6
                      (__toplevel_cons
                         5
                         (__toplevel_cons 4 (__toplevel_cons 3 (__toplevel_cons 2 (__toplevel_cons 1 ())))))))))
      (equal? (mas (listn 18) (listn 12) (listn 6)) result)))