(define number?/c (flat number?))

(define/contract (fac n)
   (-> number?/c number?/c)
   (if (= n 0)
       1
       (* n (fac (- n 1)))))

(define x (fresh))
(if (number? x)
   (fac x)
   '())
