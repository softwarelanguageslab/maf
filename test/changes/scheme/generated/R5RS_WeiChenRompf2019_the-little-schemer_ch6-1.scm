; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((atom? (lambda (x)
                  (<change>
                     (if (not (pair? x)) (not (null? x)) #f)
                     ((lambda (x) x) (if (not (pair? x)) (not (null? x)) #f)))))
         (numbered? (lambda (aexp)
                      (<change>
                         (if (atom? aexp)
                            (number? aexp)
                            (if (numbered? (car aexp))
                               (numbered? (car (cdr (cdr aexp))))
                               #f))
                         ((lambda (x) x)
                            (if (atom? aexp)
                               (number? aexp)
                               (if (<change> (numbered? (car aexp)) (not (numbered? (car aexp))))
                                  (numbered? (car (cdr (cdr aexp))))
                                  #f))))))
         (^ (lambda (n m)
              (if (zero? m) 1 (* n (^ n (- m 1))))))
         (value (lambda (nexp)
                  (if (atom? nexp)
                     nexp
                     (if (<change> (eq? (car (cdr nexp)) '+) (not (eq? (car (cdr nexp)) '+)))
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