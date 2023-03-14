#lang racket

(define (mult n m)
  (if (or (<= n 0) (<= m 0)) 0
      (+ n (mult n (- m 1)))))

(define (sqr n) (mult n n))

(provide (contract-out [mult (-> integer? integer? (and/c integer? (>=/c 0)))]
		  [sqr (->d integer? (lambda (n) (and/c integer? (>=/c n))))]))
