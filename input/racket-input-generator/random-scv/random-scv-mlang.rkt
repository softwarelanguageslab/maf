#lang racket

;;; This file provides a language extension for Racket such 
;;; that if the file starts with 
;;; #lang reader "random-scv.rkt" 
;;; every "provide" special form is replaced by code that generates random inputs
;;; for these provides and prints them.

;;; mstruct: is provided here as a replacement for Racket's struct, it adds the property #:transperent to each struct definition 
;;; mprovide: provides a replacement for Racket's "provide", as desribed above 
;;; rstruct: the original Racket struct, which is used by mstruct 
(provide (except-out (all-from-out racket) provide struct)
         (rename-out (mprovide provide) (mstruct struct) (rstruct rstruct)))

;;; We rename struct to rstruct
(require (only-in racket (struct rstruct)))
;;; We need access to Racket's match expressions while doing macro expansion
(require (for-syntax racket/match 
                     (only-in racket (struct rstruct))))

;;; A syntax expansion phase function to transform a single clause of contract-out
(define-for-syntax (transform-contract contract)
  (match contract
   [(list idn (list* '-> contract ... range)) 
    ;; we don't care about the range contract, so we only generate an input for the domain contracts
    `(displayln 
       (list (quote ,idn)
         ,@(map (lambda (c) 
                  `(for/list [(i (in-range 10))]
                      (contract-random-generate ,c)))
                contract)))]

   [(list idn contract) 
             `(displayln (list (quote ,idn) (for/list [(i (in-range 10))] 
                                       (contract-random-generate ,contract))))]
   [_ '()]))

(define-for-syntax (transform-contracts contract)
  (match contract
   [(list 'contract-out contracts ...)
    `(begin ,@(map transform-contract contracts))]))

;; A syntax expansion phase function to transform a single clause of the provide special form
(define-for-syntax (transform-clause clauses)
  (if (null? clauses) 
      '()
      (cons (match (car clauses) 
               [(list 'contract-out args ...)
                (transform-contracts (car clauses))]
               [_ (car clauses)])
            (transform-clause (cdr clauses)))))

(define-syntax (mprovide stx)
  (define datum (syntax->datum stx))
  (define clauses (match datum
    [(list _ clauses ...) 
     (transform-clause clauses)]))
  (datum->syntax stx `(begin ,@clauses)))

(define-syntax (mstruct stx)
   (define datum (syntax->datum stx))
   (datum->syntax stx (match datum
     [(list _ args ...) `(rstruct ,@args #:transparent)]
     [_ datum])))
