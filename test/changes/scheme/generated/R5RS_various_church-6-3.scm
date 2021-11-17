; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((zero (lambda (f x)
                 x))
         (inc (lambda (n)
                (<change>
                   (lambda (f x)
                      (f (n f x)))
                   ((lambda (x) x) (lambda (f x) (<change> (f (n f x)) ((lambda (x) x) (f (n f x)))))))))
         (plus (lambda (m n)
                 (lambda (f x)
                    (<change>
                       ()
                       f)
                    (m f (n f x))))))
   ((plus (inc (inc (inc zero))) (plus (inc (inc zero)) (inc zero))) (lambda (x) (+ x 1)) 0))