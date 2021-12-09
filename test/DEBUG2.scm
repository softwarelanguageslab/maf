(define test-cons-in-vec
  (lambda (a)
    (<change>
     (let ((vec (make-vector 1))
           (b 5))
         (cons a b))
     (let ((vec (make-vector 1))
           (c 5))
        (cons a c)))))

(test-cons-in-vec 5)