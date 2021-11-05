; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((atom? (lambda (x)
                  (if (not (pair? x)) (not (null? x)) #f)))
         (numbered? (lambda (aexp)
                      (<change>
                         ()
                         car)
                      (if (atom? aexp)
                         (number? aexp)
                         (if (numbered? (car aexp))
                            (numbered? (car (cdr (cdr aexp))))
                            #f))))
         (^ (lambda (n m)
              (if (zero? m)
                 (<change>
                    1
                    (* n (^ n (- m 1))))
                 (<change>
                    (* n (^ n (- m 1)))
                    1))))
         (value (lambda (nexp)
                  (if (<change> (atom? nexp) (not (atom? nexp)))
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