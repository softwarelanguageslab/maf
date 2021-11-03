; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(let ((sq (lambda (x)
            (* x x))))
   (<change>
      ()
      sq)
   (sq 2)
   (<change>
      ()
      sq)
   (sq 3))