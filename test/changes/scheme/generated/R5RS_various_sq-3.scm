; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
(let ((sq (lambda (x)
            (* x x))))
   (<change>
      (sq 2)
      ())
   (sq 3))