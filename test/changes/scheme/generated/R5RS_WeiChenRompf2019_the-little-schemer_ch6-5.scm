; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((atom? (lambda (x)
                  (if (<change> (not (pair? x)) (not (not (pair? x))))
                     (not (null? x))
                     #f)))
         (numbered? (lambda (aexp)
                      (if (atom? aexp)
                         (number? aexp)
                         (if (numbered? (car aexp))
                            (<change>
                               (numbered? (car (cdr (cdr aexp))))
                               #f)
                            (<change>
                               #f
                               (numbered? (car (cdr (cdr aexp)))))))))
         (^ (lambda (n m)
              (if (zero? m) 1 (* n (^ n (- m 1))))))
         (value (lambda (nexp)
                  (if (atom? nexp)
                     nexp
                     (if (eq? (car (cdr nexp)) '+)
                        (+ (value (car nexp)) (value (car (cdr (cdr nexp)))))
                        (if (eq? (car (cdr nexp)) '*)
                           (* (value (car nexp)) (value (car (cdr (cdr nexp)))))
                           (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))))
   (value
      (__toplevel_cons
         (__toplevel_cons 5 (__toplevel_cons '^ (__toplevel_cons 1 ())))
         (__toplevel_cons
            '*
            (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons '+ (__toplevel_cons 3 ()))) ())))))