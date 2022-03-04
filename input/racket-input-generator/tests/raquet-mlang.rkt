#lang racket
(provide (except-out (all-from-out racket) lambda)
         (rename-out [mylambda lambda]))

(define-syntax (mylambda stx)
  (displayln stx)
  #'(lambda (x) (+ x 1)))
