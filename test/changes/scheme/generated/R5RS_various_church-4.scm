; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 1
(letrec ((plus (lambda (n1 n2)
                 (lambda (f)
                    (lambda (x)
                       ((n1 f) ((n2 f) x))))))
         (mult (lambda (n1 n2)
                 (lambda (f)
                    (n2 (n1 f)))))
         (pred (lambda (n)
                 (lambda (f)
                    (lambda (x)
                       (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (ignored) x)) (lambda (id) (<change> () id) id))))))
         (sub (lambda (n1 n2)
                ((n2 pred) n1)))
         (church0? (lambda (n)
                     ((n (lambda (x) #f)) #t)))
         (church=? (lambda (n1 n2)
                     (if (church0? n1)
                        (church0? n2)
                        (if (<change> (church0? n2) (not (church0? n2)))
                           #f
                           (church=? (sub n1 church1) (sub n2 church1))))))
         (church0 (lambda (f)
                    (lambda (x)
                       x)))
         (church1 (lambda (f)
                    (lambda (x)
                       (f x))))
         (church2 (lambda (f)
                    (<change>
                       ()
                       x)
                    (lambda (x)
                       (f (f x)))))
         (church3 (lambda (f)
                    (lambda (x)
                       (<change>
                          ()
                          f)
                       (f (f (f x)))))))
   (church=?
      (mult church2 (plus church1 church3))
      (plus (mult church2 church1) (mult church2 church3))))