; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(let ((sq (lambda (x)
            (* x x))))
   (<change>
      (sq 2)
      ((lambda (x) x) (sq 2)))
   (<change>
      ()
      (sq 2))
   (sq 3))