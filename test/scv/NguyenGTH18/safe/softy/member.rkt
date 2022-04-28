#lang racket

(define (member x l)
  (if (empty? l) empty
      (if (equal? x (car l)) l (member x (cdr l)))))

(provide (contract-out
 [member (-> (any/c (listof any/c)  (listof any/c)))]))
