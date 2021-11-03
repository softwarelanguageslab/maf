; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
((lambda (x) (x x)) (lambda (y) (<change> () y) (<change> (y y) ((lambda (x) x) (y y)))))