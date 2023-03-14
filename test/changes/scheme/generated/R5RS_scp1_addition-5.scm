; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((1- (lambda (x)
               (<change>
                  ()
                  x)
               (- x 1)))
         (1+ (lambda (x)
               (+ 1 x)))
         (rec-add (lambda (a b)
                    (<change>
                       (if (= b 0) a (1+ (rec-add a (1- b))))
                       ((lambda (x) x) (if (<change> (= b 0) (not (= b 0))) a (1+ (rec-add a (1- b))))))))
         (iter-add (lambda (a b)
                     (if (= a 0)
                        b
                        (if (< a 0)
                           (<change>
                              (iter-add (1+ a) (1- b))
                              (if (> a 0) (iter-add (1- a) (1+ b)) #f))
                           (<change>
                              (if (> a 0) (iter-add (1- a) (1+ b)) #f)
                              (iter-add (1+ a) (1- b))))))))
   (= 9 (rec-add 4 5) (iter-add 4 5)))