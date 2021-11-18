; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(let ((sq (lambda (x)
            (<change>
               (* x x)
               ((lambda (x) x) (* x x))))))
   (sq 2)
   (<change>
      ()
      (display sq))
   (sq 3))