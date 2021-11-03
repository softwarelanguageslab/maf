; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
((lambda (x) (x x)) (lambda (y) (<change> () (display y)) (y y)))