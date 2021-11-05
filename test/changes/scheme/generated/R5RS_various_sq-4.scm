; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(let ((sq (lambda (x)
            (* x x))))
   (<change>
      (sq 2)
      (sq 3))
   (<change>
      (sq 3)
      (sq 2)))