; Changes:
; * removed: 0
; * added: 1
; * swaps: 1
; * negated predicates: 0
(letrec ((listn (lambda (n)
                  @sensitivity:FA
                  (if (= n 0) () (cons n (listn (- n 1))))))
         (shorterp (lambda (x y)
                     @sensitivity:FA
                     (<change>
                        ()
                        (display shorterp))
                     (if (not (null? y))
                        (let ((__or_res (null? x)))
                           (if __or_res __or_res (shorterp (cdr x) (cdr y))))
                        #f)))
         (mas (lambda (x y z)
                (<change>
                   @sensitivity:FA
                   (if (not (shorterp y x))
                      z
                      (mas (mas (cdr x) y z) (mas (cdr y) z x) (mas (cdr z) x y))))
                (<change>
                   (if (not (shorterp y x))
                      z
                      (mas (mas (cdr x) y z) (mas (cdr y) z x) (mas (cdr z) x y)))
                   @sensitivity:FA))))
   (let ((result (__toplevel_cons
                   7
                   (__toplevel_cons
                      6
                      (__toplevel_cons
                         5
                         (__toplevel_cons 4 (__toplevel_cons 3 (__toplevel_cons 2 (__toplevel_cons 1 ())))))))))
      (equal? (mas (listn 18) (listn 12) (listn 6)) result)))