(define (number->string n)
  (if (= n 1)
      "1"
      "-1"))

(define (string->number s)
  (if (eq? "a" s)
      1
      2))

(number->string 1)
(string->number "a")