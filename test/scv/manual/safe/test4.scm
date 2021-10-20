; CROSS COMPONENT PATH CONDITION
; NOTE: this does only check incoming flow, not outgoing flow, which might also be of interest.

(define real?/c (flat real?))
(define any?/c (flat (lambda (v) #t)))

(define/contract (foo x)
   (-> real?/c real?/c)
   x)

(define (bar x)
  (foo x))

(define/contract (baz x)
   (-> real?/c any?/c)
   (bar x))

(define x (fresh))
(if (real? x)
   (baz x)
   '())

