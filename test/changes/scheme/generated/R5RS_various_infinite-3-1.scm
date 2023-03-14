; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
((lambda (x) (x x)) (lambda (y) (<change> () (display (y y))) (<change> () (y y)) (y y)))