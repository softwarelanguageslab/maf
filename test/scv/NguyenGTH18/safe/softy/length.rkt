#lang racket

(define (len xs)
  (if (empty? xs) 0
      (+ 1 (len (cdr xs)))))

(provide (contract-out 
 [len (-> (listof any/c) (and/c integer? (>=/c 0)))]))
