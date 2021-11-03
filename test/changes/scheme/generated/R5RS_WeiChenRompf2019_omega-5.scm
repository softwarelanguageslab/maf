; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
((lambda (x) (x x)) (lambda (x) (<change> () x) (<change> () (x x)) (x x)))