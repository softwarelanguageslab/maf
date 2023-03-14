#lang racket
(require soft-contract/fake-contract)

(struct node (v l r))

(define (braun-tree? x)
  (or (false? x)
      (and (node? x)
           (braun-tree? (node-l x))
           (braun-tree? (node-r x))
           (let ([l (size (node-l x))]
                 [r (size (node-r x))])
             (or (= l r) (= l (add1 r)))))))

(define (size x)
  (if (node? x)
      (add1 (+ (size (node-l x)) (size (node-r x))))
      0))

(define (insert bt x)
  (if (node? bt)
      (node (node-v bt) (insert (#|HERE|#node-l bt) x) (node-r bt))
      (node x #f #f)))

(provide/contract
 [braun-tree? (any/c . -> . boolean?)]
 [insert (braun-tree? any/c . -> . braun-tree?)])
