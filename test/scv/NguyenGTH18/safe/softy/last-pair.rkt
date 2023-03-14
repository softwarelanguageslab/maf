#lang racket

(define (lastpair x)
  (if (pair? (cdr x)) (lastpair (cdr x)) x))

(provide (contract-out 
   [lastpair (-> pair? pair?)]))
