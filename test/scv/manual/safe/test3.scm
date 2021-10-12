
(define real?/c (flat real?))

(define foo
   (mon (-> real?/c real?/c) 
        (lambda (x)
           (if (real? x)
               x
               #f))))

(define x (fresh))
(if (real? x) 
    (foo x)
    '())
