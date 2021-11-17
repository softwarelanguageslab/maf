; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((loop (lambda (i l)
                 (if (let ((__or_res (eq? 6 1))) (if __or_res __or_res (let ((__or_res (eq? 6 2))) (<change> (if __or_res __or_res (let ((__or_res (eq? 6 3))) (if __or_res __or_res (let ((__or_res (eq? 6 4))) (if __or_res __or_res (eq? 6 5)))))) ((lambda (x) x) (if (<change> __or_res (not __or_res)) __or_res (let ((__or_res (eq? 6 3))) (if __or_res __or_res (let ((__or_res (eq? 6 4))) (if __or_res __or_res (eq? 6 5)))))))))))
                    #f
                    (if (eq? 6 6) (if (< i l) (loop (+ 1 i) l) l) #f)))))
   (<change>
      (loop 0 20000)
      ((lambda (x) x) (loop 0 20000))))