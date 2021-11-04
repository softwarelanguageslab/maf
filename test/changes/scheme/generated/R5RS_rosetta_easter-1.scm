; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((easter (lambda (year)
                   (let* ((a (remainder year 19))
                          (b (quotient year 100))
                          (c (remainder year 100))
                          (d (quotient b 4))
                          (e (remainder b 4))
                          (f (quotient (+ b 8) 25))
                          (g (quotient (+ 1 (- b f)) 3))
                          (h (remainder (+ (* 19 a) (- b d g) 15) 30))
                          (i (quotient c 4))
                          (k (remainder c 4))
                          (l (remainder (+ e e i i (- 32 h k)) 7))
                          (m (quotient (+ a (* 11 h) (* 22 l)) 451))
                          (n (+ h l (- 114 (* 7 m)))))
                      (<change>
                         (list (quotient n 31) (+ 1 (remainder n 31)))
                         ((lambda (x) x) (list (quotient n 31) (+ 1 (remainder n 31)))))))))
   (if (equal? (easter 2017) (__toplevel_cons 4 (__toplevel_cons 16 ())))
      (<change>
         (if (equal? (easter 1027) (__toplevel_cons 4 (__toplevel_cons 1 ())))
            (if (equal? (easter 2016) (__toplevel_cons 3 (__toplevel_cons 27 ())))
               (equal? (easter 172) (__toplevel_cons 3 (__toplevel_cons 29 ())))
               #f)
            #f)
         #f)
      (<change>
         #f
         (if (not (equal? (easter 1027) (__toplevel_cons 4 (__toplevel_cons 1 ()))))
            (if (equal? (easter 2016) (__toplevel_cons 3 (__toplevel_cons 27 ())))
               (equal? (easter 172) (__toplevel_cons 3 (__toplevel_cons 29 ())))
               #f)
            #f))))