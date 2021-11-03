; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 0
(letrec ((fib (lambda (n)
                (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))
   (fib 10))