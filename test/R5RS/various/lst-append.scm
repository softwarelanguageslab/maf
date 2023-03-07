;taken from SICP 1, a program that makes heavy use of the built-in append procedure

(define (rec-sum-lists l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (cons (+ (car l1) (car l2))
                    (rec-sum-lists (cdr l1) (cdr l2))))))

(define (rec-merge-n lst1 lst2 n)
  (define (geef-n+rest lst n)
    (cond ((or (= 0 n) (null? lst)) (cons '() lst))
          (else (let* ((res (geef-n+rest (cdr lst) (- n 1)))
                       (first (car res))
                       (rest (cdr res)))
                  (cons (cons (car lst) first) rest)))))

  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else
         (let* ((n-lst1 (geef-n+rest lst1 n))
                (n-lst2 (geef-n+rest lst2 n))
                (n-lst1-first (car n-lst1))
                (n-lst1-rest (cdr n-lst1))
                (n-lst2-first (car n-lst2))
                (n-lst2-rest (cdr n-lst2)))
           (append (append n-lst1-first n-lst2-first)
                   (rec-merge-n n-lst1-rest n-lst2-rest n))))))

(rec-merge-n '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22) '(23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38) 2)