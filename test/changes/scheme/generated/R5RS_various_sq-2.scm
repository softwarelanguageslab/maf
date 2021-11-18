; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(let ((sq (lambda (x)
            (<change>
               ()
               x)
            (* x x))))
   (<change>
      (sq 2)
      ())
   (sq 3))