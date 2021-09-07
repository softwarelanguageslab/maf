(define (phi x2)
  (<change> (not x2) x2))
(or (phi #t) (phi #f))
