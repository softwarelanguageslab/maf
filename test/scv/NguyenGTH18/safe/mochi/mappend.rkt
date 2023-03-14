#lang racket

(define (mappend xs ys)
  (if (empty? xs) ys
      (cons (car xs) (mappend (cdr xs) ys))))

(define (map-append f xs)
  (if (empty? xs) empty
      (mappend (f (car xs)) (map-append f (cdr xs)))))

(provide (contract-out 
 [map-append (-> (-> any/c (listof any/c)) (listof any/c) (listof any/c))]
 [mappend (-> (listof any/c) (listof any/c) (listof any/c))]))
