(define (a t)
  (if (not (pair? t))
         t
        (cons (b (car t))
              (a (cdr t)))))

(define (b sym)
  (let ((x (assq sym l1)))
    (if x
        (cdr x)
        (begin (set! l1 (list (list sym sym)))
               (list sym)))))

(define l1 '())

(define (c t lst)
  (d t (car lst))
  (c (car lst) '()))

(define l2 '())

(define (d t1 t2)
  (if (not (pair? t2))
      (begin
        (assq t2 l2)
        (<change> #t #f) ; <<==============================================
        (set! l2 (cons (cons t1 t1)
                                l2)))))

(define t (a (list '())))
(c t (car t))