;;; DESTRUC -- Destructive operation benchmark.

(define (append-to-tail! x y)
  @sensitivity:FA
  (if (null? x)
      y
      (let loop ((a x) (b (cdr x)))
        @sensitivity:FA
        (if (null? b)
            (begin
              (set-cdr! a y)
              x)
            (loop b (cdr b))))))

(define (destructive n m)
  @sensitivity:FA
  (let ((l (do ((i 10 (- i 1)) (a '() (cons '() a)))
               ((= i 0) a)
             @sensitivity:FA)))
    (do ((i n (- i 1)))
        ((= i 0) l)
      @sensitivity:No ;; does not terminate with FA
      (cond ((null? (car l))
             (do ((l l (cdr l)))
                 ((null? l))
               @sensitivity:FA
               (if (null? (car l)) (set-car! l (cons '() '())))
               (append-to-tail! (car l)
                                (do ((j m (- j 1)) (a '() (cons '() a)))
                                    ((= j 0) a)
                                  @sensitivity:No ;; does not terminate with FA, 1A
                                  ))))
            (else
             (do ((l1 l (cdr l1)) (l2 (cdr l) (cdr l2)))
                 ((null? l2))
               @sensitivity:FA
               (set-cdr! (do ((j (quotient (length (car l2)) 2) (- j 1))
                              (a (car l2) (cdr a)))
                             ((zero? j) a)
                           @sensitivity:FA
                           (set-car! a i))
                         (let ((n (quotient (length (car l1)) 2)))
                           (cond ((= n 0)
                                  (set-car! l1 '())
                                  (car l1))
                                 (else
                                  (do ((j n (- j 1)) (a (car l1) (cdr a)))
                                    ((= j 1)
                                     (let ((x (cdr a)))
                                       (set-cdr! a '())
                                       x))
                                    @sensitivity:FA
                                    (set-car! a i))))))))))))

(equal? (destructive 600 50)
        '((1 1 2)
          (1 1 1)
          (1 1 1 2)
          (1 1 1 1)
          (1 1 1 1 2)
          (1 1 1 1 2)
          (1 1 1 1 2)
          (1 1 1 1 2)
          (1 1 1 1 2)
          (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3)))
