(define (run f)
  (or (f #t) (f #t)))
(run (<change> (lambda (x) (not x))
               (lambda (x) x)))