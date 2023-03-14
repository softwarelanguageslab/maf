#lang racket
(require soft-contract/fake-contract)

(define (positive? x)
  (and (integer? x) (<= 0 x)))

(define (divides? a b)
  (if (= 0 b) #t
      (if (< b a) 
          #f
          (divides? a (- b a)))))

(provide (contract-out [divides? (-> positive? positive? boolean?)]))
