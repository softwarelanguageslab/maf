#lang racket/base

(require (only-in typed/racket/base index?)
         racket/contract/base)

(define (f x) (if (index? x) (+ 1 x) 42.0))

(provide
 (contract-out
  [f (any/c . -> . (not/c exact?))]))
