; lifo != fifo (incremental) 
(letrec ((for (lambda (f)
                (<change>
                  (f 1)
                  ())))
          (make-matrix (lambda (init)
                         (for (lambda (j) (init () ())))))
          (make-maze (make-matrix (lambda (i j) (<change> #t #f)))))
  ())

