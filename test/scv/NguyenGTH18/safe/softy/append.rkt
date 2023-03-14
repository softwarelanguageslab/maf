#lang racket

(define (append xs ys)
  (if (empty? xs) ys
      (cons (car xs) (append (cdr xs) ys))))

(provide (contract-out
   [append (-> (listof any/c) (listof any/c) (listof any/c))]))
