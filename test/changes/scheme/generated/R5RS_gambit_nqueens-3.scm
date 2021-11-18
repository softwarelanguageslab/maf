; Changes:
; * removed: 1
; * added: 0
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((one-to (lambda (n)
                   (letrec ((loop (lambda (i l)
                                    (if (= i 0) l (loop (- i 1) (cons i l))))))
                      (<change>
                         (loop n ())
                         ((lambda (x) x) (loop n ()))))))
         (ok? (lambda (row dist placed)
                (if (null? placed)
                   #t
                   (if (not (= (car placed) (+ row dist)))
                      (if (not (= (car placed) (- row dist)))
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
   (<change>
      (nqueens 8)
      ())
   #t)