#lang racket

(define (lock st) 1)
(define (unlock st) 0)
(define (f n st) (if (> n 0) (lock st) st))
(define (g n st) (if (> n 0) (unlock st) st))
(define (main n) (g n (f n 0)))

(provide (contract-out
 [main (-> integer? (one-of/c 0))]))
