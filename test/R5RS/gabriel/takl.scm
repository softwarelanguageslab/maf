(define (listn n)
  @sensitivity:FA
  (if (= n 0)
      '()
      (cons n (listn (- n 1)))))
(define (shorterp x y)
  @sensitivity:FA
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))
(define (mas x y z)
  @sensitivity:FA
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))
(let ((result '(7 6 5 4 3 2 1)))
  (equal? (mas (listn 18) (listn 12) (listn 6)) result))
