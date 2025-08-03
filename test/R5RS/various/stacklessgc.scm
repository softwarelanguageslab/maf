(define (f n)
    (if (= n 6)
        n
        (let ((r (f (+ n 1))))
            (+ r n))))

(define res (f 5))
res 