(define lst (cons (<change> 1.5 1) '()))
(set-car! lst (+ (car lst) 1))
(assert (integer? (car lst)))