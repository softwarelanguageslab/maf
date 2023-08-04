(letrec ((for-each (lambda (f l)
                     (f (car l))))
          (*p* (make-vector 1)))
  (for-each
    (lambda (i)
      (<change>
        (vector-set! *p* i (make-vector 1))
        (vector-set! *p* i 1)))
    (cons 1 ())))
