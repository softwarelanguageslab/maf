; Modified from https://github.com/philnguyen/soft-contract/blob/a6a2963bb3d5826ca4405a257a6178873d046b76/soft-contract/test/programs/safe/larceny/primes.rkt

;;; PRIMES -- Compute primes less than n, written by Eric Mohr.

(define  (interval-list m n)
  (if (> m n)
      '()
      (cons m (interval-list (+ 1 m) n))))

(define (sieve l)
  (letrec ((remove-multiples
            (lambda (n l)
              (if (null? l)
                  '()
                  (if (<change> (> (remainder (car l) n) 0) (= (remainder (car l) n) 0)) ; <<====
                      (remove-multiples n (cdr l))
                      (cons (car l)
                            (remove-multiples n (cdr l))))))))
    (if (null? l)
        '()
        (cons (car l)
              (sieve (remove-multiples (car l) (cdr l)))))))

(define (primes<= n)
  (assert (integer? n))
  (let ((result (sieve (interval-list 2 n))))
    (assert (and (list? result)
                 (let loop ((l result))
                   (cond ((null? l) #t)
                         ((not (and (integer? (car l))
                                    (positive? (car l)))) #f)
                         (else (loop (cdr l)))))))
    result))

(primes<= 1000)