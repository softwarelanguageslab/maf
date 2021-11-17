; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((fib (lambda (n)
                (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))
   (<change>
      ()
      (display 10))
   (<change>
      ()
      (display fib))
   (fib 10))