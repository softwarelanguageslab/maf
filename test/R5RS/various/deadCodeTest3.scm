(define (dead-l x y z)
  (+ x y)
  (+ y z)
  (+ z x))

(define (alive a b c)
  (+ a b))


(alive 3 2 0)
(alive 3 2 dead-l)

(if #t
    (alive 1 2 3)
    (begin
      (alive 9 9 9)
      (alive 8 8 8)))