#lang racket

(define (append xs ys)
  (if (empty? xs) ys
      (cons (car xs) (append (cdr xs) ys))))

(define (map-append f xs)
  (if (empty? xs) empty
      (append (f (car xs)) (map-append f (cdr xs)))))

(provide (contract-out 
 [map-append (-> (-> any/c (listof any/c)) (listof any/c) (listof any/c))]
 [append (-> (listof any/c) (listof any/c) (listof any/c))]))
