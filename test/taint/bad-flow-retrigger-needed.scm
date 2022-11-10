; If a is analysed first, f will be called with #t. If b is analysed later, it's implicit flows will only reach f if f is forcibly reanalysed.
(define (f x) (sink x))
(define (a) (f #t))
(define (b)
  (define v #t)
  (define v-s (source v))
  (if v-s (f v-s)))
(a)
(b)