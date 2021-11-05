; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((loop (lambda (i l)
                 (if (let ((__or_res (eq? 6 1))) (<change> () (display eq?)) (if __or_res __or_res (let ((__or_res (eq? 6 2))) (if __or_res __or_res (let ((__or_res (eq? 6 3))) (if __or_res (<change> __or_res (let ((__or_res (eq? 6 4))) (if __or_res __or_res (eq? 6 5)))) (<change> (let ((__or_res (eq? 6 4))) (if __or_res __or_res (eq? 6 5))) __or_res)))))))
                    #f
                    (if (<change> (eq? 6 6) (not (eq? 6 6)))
                       (if (< i l) (loop (+ 1 i) l) l)
                       #f)))))
   (loop 0 20000))