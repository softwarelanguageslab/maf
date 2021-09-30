(define number?/c (flat number?))
(define any?/c (flat (lambda (v) #t)))
(define/contract (add1 x)
   (-> number?/c number?/c)
   (+ x 1))

(define/contract (f x)
  (-> any?/c number?/c)
  (if (number? x) (add1 x) 0))


(f (fresh))
