#lang racket

(define (mc91 x)
  (if (> x 100) (- x 10)
      (mc91 (mc91 (+ x 11)))))

(provide (contract-out
   [mc91 (->i ([n integer?]) (res (n) (and/c integer? (λ (a) (implies (<= n 101) (= a 91))))))]))
