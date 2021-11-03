; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(let ((sq (lambda (x)
            (* x x))))
   (<change>
      ()
      3)
   (sq 2)
   (sq 3))