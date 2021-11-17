; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(let ((res ((lambda (f1) (letrec ((a (f1 #t))) (f1 #f)))
             (lambda (x1)
                (<change>
                   ()
                   #t)
                ((lambda (f2)
                   (letrec ((b (f2 #t))
                            (c (f2 #f)))
                      (f2 #t)))
                   (lambda (x2)
                      ((lambda (z) (z x1 x2)) (lambda (y1 y2) y1))))))))
   (<change>
      ()
      res)
   res)