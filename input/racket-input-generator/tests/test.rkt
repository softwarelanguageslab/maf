#lang reader "raquet.rkt"
(define identity (lambda (x) x))
(displayln (identity 5))
(provide identity)
