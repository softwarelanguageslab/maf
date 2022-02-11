#lang racket

;;; This file provides a language extension for Racket such 
;;; that if the file starts with 
;;; #lang reader "random-scv.rkt" 
;;; every "provide" special form is replaced by code that generates random inputs
;;; for these provides and prints them.

;;; mstruct: is provided here as a replacement for Racket's struct, it adds the property #:transperent to each struct definition 
;;; mprovide: provides a replacement for Racket's "provide", as desribed above 
;;; rstruct: the original Racket struct, which is used by mstruct 
(provide (except-out (all-from-out racket) provide struct ->d)
         (rename-out (mprovide provide) (mstruct struct) (rstruct rstruct) (r->d r->d) (m->d ->d)))

;;; We rename struct to rstruct
(require (only-in racket (struct rstruct) (->i r->d)))
;;; We need access to Racket's match expressions while doing macro expansion
(require (for-syntax racket/match 
                     (only-in racket (struct rstruct))))

;;; A replacement for Racket's ->d. 
;;; Sytnax: 
;;; (->d domain-contract ... dependent-range) 
;;; 
;;; 
;;; For example:
;;; (-> number? number? (lambda (x y) (>/c (+ x y))))
;;;
;;; is translated to: 
;;; 
;;; (r->d ([gensym1 number?]
;;;        [gensym2 number?])
;;;   [result (gensym1 gensym2) ((lambda (x y) (>/c (+ x y))) gensym1 gensym2)])
(define-syntax (m->d stx)
  (define datum (syntax->datum stx))
  (datum->syntax stx (match datum 
   [(list* _ domains ... (list rangeMaker))
    (let* 
      ((names (for/list [(_ domains)] (gensym)))
       (new-domains (map (lambda (domain name) `(,name ,domain)) domains names))
       (new-range `(,rangeMaker ,@names)))

      `(r->d ,new-domains (result ,names ,new-range)))])))

;;; A syntax expansion phase function to transform a single clause of contract-out
(define-for-syntax (transform-contract contract)
  (match contract
   [(list idn (list* '-> contract ... range)) 
    ;; we don't care about the range contract, so we only generate an input for the domain contracts
    `(displayln 
       (list (quote ,idn)
         ,@(map (lambda (c) 
                  `(for/list [(i (in-range 10))]
                      (with-handlers ([exn:fail? (lambda (e) 'fail)]) (contract-random-generate ,c))))
                contract)))]

   [(list idn contract) 
             `(displayln (list (quote ,idn) (for/list [(i (in-range 10))] 
                                       (with-handlers ([exn:fail? (lambda (e) 'fail)]) (contract-random-generate ,contract)))))]
   [_ '(void)]))

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
     [(list _ args ...) (if (member '#:transparent args) `(rstruct ,@args) `(rstruct ,@args #:transparent))]
     [_ datum])))
