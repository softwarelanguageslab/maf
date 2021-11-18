; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
((lambda (f1) (let ((a (f1 #t))) (f1 #f)))
   (lambda (x1)
      ((lambda (f2) (let ((b (f2 #t))) (f2 #f)))
         (lambda (x2)
            (<change>
               ()
               (display x2))
            ((lambda (f3) (let ((c (f3 #t))) (f3 #f)))
               (lambda (x3)
                  (<change>
                     ((lambda (z) (z x1 x2 x3)) (lambda (y1 y2 y3) y1))
                     ((lambda (x) x) ((lambda (z) (z x1 x2 x3)) (lambda (y1 y2 y3) y1))))))))))