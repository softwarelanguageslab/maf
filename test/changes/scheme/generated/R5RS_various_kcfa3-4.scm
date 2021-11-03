; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(let ((res ((lambda (f1) (letrec ((a (f1 #t))) (f1 #f)))
             (lambda (x1)
                ((lambda (f2) (letrec ((b (f2 #t))) (f2 #f)))
                   (lambda (x2)
                      ((lambda (f3) (letrec ((c (f3 #t))) (f3 #f)))
                         (lambda (x3)
                            (<change>
                               ()
                               (display y2))
                            ((lambda (z) (z x1 x2 x3)) (lambda (y1 y2 y3) y1))))))))))
   res)