; 2.1
(define (sign number)
  (cond ((zero? number) 0)
    ((> number 0) 1)
    (else -1)))

(define (divides? deler deeltal)
  (= 0  (modulo deeltal deler)))

(define (leap-year? year)
  (if (divides? 4 year)
    (if (divides? 100 year)
      (divides? 400 year)
      #t)
    #f))

(and (not (or (leap-year? 1989)
            (leap-year? 1900)))
  (leap-year? 2000)
  (= -1 (sign -5))
  (= 1 (sign 17.28))
  (= 0 (sign 0)))

; 2.4
(define (derde-machtswortel x)
  (define epsilon 0.01)
  (define (hulp-derde-machtswortel y)
    (if (< (abs (- (* y y y) x)) epsilon)
      y
      (hulp-derde-machtswortel (/ (+ (/ x (* y y)) y y) 3))))
  (hulp-derde-machtswortel (/ x 3)))

(= 3.000000068671529 (exact->inexact (derde-machtswortel 27)))