; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((fib (lambda (n)
                (<change>
                   ()
                   (< n 2))
                (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))
   (fib 10))