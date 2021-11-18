; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((one-to (lambda (n)
                   (letrec ((loop (lambda (i l)
                                    (if (= i 0) l (loop (- i 1) (cons i l))))))
                      (<change>
                         ()
                         (display n))
                      (loop n ()))))
         (ok? (lambda (row dist placed)
                (if (null? placed)
                   #t
                   (if (not (= (car placed) (+ row dist)))
                      (if (<change> (not (= (car placed) (- row dist))) (not (not (= (car placed) (- row dist)))))
                         (ok? row (+ dist 1) (cdr placed))
                         #f)
                      #f))))
         (try-it (lambda (x y z)
                   (if (null? x)
                      (if (null? y) 1 0)
                      (+
                         (if (ok? (car x) 1 z)
                            (try-it (append (cdr x) y) () (cons (car x) z))
                            0)
                         (try-it (cdr x) (cons (car x) y) z)))))
         (nqueens (lambda (n)
                    (try-it (one-to n) () ()))))
   (nqueens 8)
   #t)