; Modified from https://github.com/philnguyen/soft-contract/blob/a6a2963bb3d5826ca4405a257a6178873d046b76/soft-contract/test/programs/safe/mochi/fold-fun-list.rkt

; init - incr - rean
; v = verified, x = failed

(define (mk-list n)
  (assert (integer? n)) ; x x v                                     /=
  (if (<= n 0)
      '()
      (cons (lambda (m)
              (assert (integer? m)) ; x x v                         /=
              (+ m n))
            (mk-list (- n 1)))))

(define (foldr f z xs)
  (assert (procedure? f)) ; v v v                                   ==
  (assert (procedure? z)) ; v v v                                   ==
  (assert (list? xs)) ; x x x                                       ==
  (if (null? xs) z (f (car xs) (foldr f z (cdr xs)))))

(define (compose f g) (lambda (x) (f (g x))))

(define (main n)
  (assert (integer? n)) ; x v v                                     ==
  (let ((xs (mk-list n)))
    (assert (and (list? xs) ; x x x                                 ==
                 (let loop ((l xs))
                   (cond ((null? l) #t)
                         ((not (procedure? (car l))) #f)
                         (else (loop (cdr l)))))))
    (foldr compose (lambda (x) x) xs)))

(define result (main (<change> 10.5 10)))
(assert (>= (result 0) 0)) ; x x x                                  ==

; (number? x) (in <=) x x v                                         /=
; (number? x) (in >=) x x v                                         /=
; (number? x) (in > ) x x v                                         /=