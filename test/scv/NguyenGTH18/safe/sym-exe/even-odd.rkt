;; OK  but changed even? to meven? and odd? to modd?
;; TODO check why defining odd? and even? do not work
;; (1) allow redefinition of primitives or
;; (2) throw an error when the user redfines a function
;; (3) check what racket does and implement that

(define (meven? n)
  (if (zero? n) #t (modd? (sub1 n))))

(define (modd? n)
  (if (zero? n) #f (meven? (sub1 n))))


(provide/contract
 (meven? (-> number?  bool?))
 (modd? (-> number? bool?)))

(@unchecked meven? OPQ)
(@unchecked modd? OPQ)
(safe)
