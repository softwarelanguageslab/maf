(define (mmap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (mmap f (cdr lst)))))

(mmap car '((a 1)))
(mmap cdr '((b 2)))
