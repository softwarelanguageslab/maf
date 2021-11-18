; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((zero (lambda (f x)
                 x))
         (inc (lambda (n)
                (lambda (f x)
                   (f (n f x)))))
         (plus (lambda (m n)
                 (<change>
                    ()
                    (lambda (f x)
                       (m f (n f x))
                       (m f (n f x))))
                 (lambda (f x)
                    (m f (n f x))))))
   ((plus (inc (inc (inc zero))) (plus (inc (inc zero)) (inc zero))) (lambda (x) (+ x 1)) 0))