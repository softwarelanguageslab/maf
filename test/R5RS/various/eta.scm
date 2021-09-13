;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: #f
(define (do-something) 10)
(define (id y)
  (do-something)
  y)
(define r1 ((id (lambda (a) a)) #t))
(define r2 ((id (lambda (b) b)) #f))
r1 
