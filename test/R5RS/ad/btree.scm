(define (make-node key left right parent info)
  (list key left right parent info))

(define (key node)
  (list-ref node 0))

(define (left node)
  (list-ref node 1))

(define (right node)
  (list-ref node 2))

(define (parent node)
  (list-ref node 3))

(define (info node)
  (list-ref node 4))

(define (set-key! node key)
  (set-car! node key))

(define (set-left! node l)
  (set-car! (cdr node) l))

(define (set-right! node r)
  (set-car! (cddr node) r))

(define (set-parent! node p)
  (set-car! (cdddr node) p))

(define (set-info! node i)
  (set-car! (cddddr node) i))

(define null-tree '())

(define (null-tree? tree)
  (null? tree))

(define (is-leaf? node)
  (and (null-tree? (left node))
       (null-tree? (right node))))

(define (is-root? node)
  (null-tree? (parent node)))

;; Added a body.

(define root (make-node 20 null-tree null-tree null-tree "The root"))
(define l (make-node 10 null-tree null-tree root "l"))
(set-left! root l)
(define r (make-node 30 null-tree null-tree root "r"))
(set-right! root r)
(define ll (make-node 5 null-tree null-tree l "ll"))
(set-left! l ll)
(define lr (make-node 15 null-tree null-tree l "lr"))
(set-right! l lr)
(define rl (make-node 25 null-tree null-tree r "rl"))
(set-left! r rl)
(define rr (make-node 35 null-tree null-tree r "rr"))
(set-right! r rr)
(equal? (info (vector-ref (vector root l ll lr r rl rr) 3)) (info root))
(and (is-leaf? rr) (is-leaf? root))
(null-tree? root)
(set-info! l "lol")
(set-parent! ll rr)
(set-left! rr ll)
(<= (key (left rr)) 10)