#lang racket

(define (mk-list n x)
  (if (< n 0) empty (cons x (mk-list (- n 1) x))))

(define (mem x xs)
  (if (empty? xs) #f (or (= x (car xs)) (mem x (cdr xs)))))

(provide (contract-out
   [mk-list (->d integer? integer? 
                 (lambda (_ x) 
                   (and/c (listof integer?)
                          (lambda (l) (or (empty? l) (member x l))))))]
   [mem (-> integer? (listof integer?) boolean?)]))
