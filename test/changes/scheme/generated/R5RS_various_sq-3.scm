; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(let ((sq (lambda (x)
            (<change>
               ()
               (display x))
            (<change>
               ()
               x)
            (* x x))))
   (sq 2)
   (sq 3))