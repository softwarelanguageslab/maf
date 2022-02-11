#lang reader random-scv

(struct posn (x y))
(define POSN/C (struct/c posn number? number?))
(define (foo pos) pos)
(provide (contract-out [x POSN/C]
                       [foo (-> number? POSN/C POSN/C)]))
