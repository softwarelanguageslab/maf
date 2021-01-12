; Modified from https://github.com/philnguyen/soft-contract/blob/a6a2963bb3d5826ca4405a257a6178873d046b76/soft-contract/test/programs/safe/larceny/ntakl.rkt

;;; NTAKL -- The TAKeuchi function using lists as counters,
;;; with an alternative boolean expression.

(define (listn n)
  (if (= n 0)
      '()
      (cons n (listn (<change> (+ n 1) (- n 1)))))) ; <<=====

(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))

(define (mas x y z)
  (assert (and (list? x)
               (list? y)
               (list? z)))
  (let ((result (if (not (shorterp y x))
                    z
                    (mas (mas (cdr x) y z)
                         (mas (cdr y) z x)
                         (mas (cdr z) x y)))))
    (assert (list? result))
    result))

(define (shorterp x y)
 (<change>
      (and (not (null? y))
           (or (null? x)
               (shorterp (cdr x)
                         (cdr y))))
      (cond ((null? y) #f)
            ((null? x) #t)
            (else
              (shorterp (cdr x) (cdr y))))))

(mas l18 l6 l12)