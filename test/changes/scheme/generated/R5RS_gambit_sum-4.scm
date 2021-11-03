; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 0
(letrec ((run (lambda (n)
                ((letrec ((loop (lambda (i sum) (if (< i 0) sum (loop (- i 1) (+ i sum)))))) loop) n 0))))
   (= (run 10000) 50005000))