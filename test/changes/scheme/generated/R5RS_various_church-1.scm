; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 3
(letrec ((plus (lambda (n1 n2)
                 (lambda (f)
                    (lambda (x)
                       ((n1 f) ((n2 f) x))))))
         (mult (lambda (n1 n2)
                 (<change>
                    (lambda (f)
                       (n2 (n1 f)))
                    ((lambda (x) x) (lambda (f) (n2 (n1 f)))))))
         (pred (lambda (n)
                 (lambda (f)
                    (lambda (x)
                       (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (ignored) x))
                          (lambda (id)
                             (<change>
                                id
                                ((lambda (x) x) id))))))))
         (sub (lambda (n1 n2)
                ((n2 pred) n1)))
         (church0? (lambda (n)
                     ((n (lambda (x) #f)) #t)))
         (church=? (lambda (n1 n2)
                     (if (church0? n1)
                        (church0? n2)
                        (if (church0? n2)
                           #f
                           (church=? (sub n1 church1) (sub n2 church1))))))
         (church0 (lambda (f)
                    (lambda (x)
                       x)))
         (church1 (lambda (f)
                    (lambda (x)
                       (<change>
                          ()
                          (display f))
                       (f x))))
         (church2 (lambda (f)
                    (lambda (x)
                       (f (f x)))))
         (church3 (lambda (f)
                    (lambda (x)
                       (f (f (f x)))))))
   (<change>
      (church=?
         (mult church2 (plus church1 church3))
         (plus (mult church2 church1) (mult church2 church3)))
      ((lambda (x) x)
         (church=?
            (mult church2 (plus church1 church3))
            (plus (mult church2 church1) (mult church2 church3))))))