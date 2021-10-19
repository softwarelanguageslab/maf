; CROSS COMPONENT PATH CONDITION

(define real?/c (flat real?))

(define/contract (foo x)
   (-> real?/c real?/c)
   x)

(define (bar x)
  (foo x))

(define/contract (baz x)
   (-> real?/c real?/c)
   (bar x))

(define x (fresh))
(if (real? x)
   (baz x)
   '())

