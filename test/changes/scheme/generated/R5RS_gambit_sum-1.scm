; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((run (lambda (n)
                (<change>
                   ((letrec ((loop (lambda (i sum) (if (< i 0) sum (loop (- i 1) (+ i sum)))))) loop) n 0)
                   ((lambda (x) x)
                      ((letrec ((loop (lambda (i sum) (if (< i 0) sum (loop (- i 1) (+ i sum)))))) loop) n 0))))))
   (<change>
      ()
      (display (run 10000)))
   (<change>
      (= (run 10000) 50005000)
      ((lambda (x) x) (= (run 10000) 50005000))))