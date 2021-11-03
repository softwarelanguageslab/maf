; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(let ((sq (lambda (x)
            (* x x))))
   (sq 2)
   (<change>
      ()
      (display sq))
   (sq 3))