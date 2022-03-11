;; TODO 
;; the issue here is that it has no knowledge about the or when analysing the function
;; so it cannot verify this correctly
;

(define number?/c (flat number?))
(define string?/c (flat string?))
(define any?/c (flat (lambda (v) #t)))
(define (or/c contract1 contract2) 
  (flat (lambda (v) (or (check contract1 v) (check contract2 v)))))

(define/contract (add1 x)
   (-> number?/c number?/c)
   (+ x 1))

(define/contract (f x)
  (-> (or/c string?/c number?/c) number?/c)
  (if (number? x) (add1 x) (string-length x)))

(f (fresh))
