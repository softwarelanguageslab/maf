; Demonstrates shortcoming in Phil's email from 7/13
#lang racket
(require soft-contract/fake-contract)

(define phil
  (lambda (l1)
    (lambda (l2)
      (lambda (l3)
	l1))))

(provide/contract
 [phil  (->i ([l1 number?])
	     (res (l1)
		  (->i ([l2 number?])
		       (res (l2)
			    (->i ([l3 (and/c number? (not/c zero?) (=/c (* l1 l2)))])
				 (res (l3) (not/c zero?)))))))])
