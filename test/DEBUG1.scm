(define test-cons-in-vec
  (lambda (a)
    (<change>
     (let ((vec (make-vector 1))
           (b 5))
       (vector-set! vec 0 (cons a b)))
     (let ((vec (make-vector 1))
           (c 5))
       (vector-set! vec 0 (cons a c))))))

(test-cons-in-vec 5)