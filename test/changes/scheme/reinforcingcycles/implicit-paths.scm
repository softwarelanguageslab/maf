; Requires implicit paths for conditionals to find a SCA.
(define (phi x)
  (<change> (not x) x))
(or (phi #t) (phi #f))
