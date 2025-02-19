; WCR 3-5 reductie
(letrec ((fun
         (lambda (l)
           (if (null? l)
             (fun l)
             (<change> (fun (cdr l)) ())))))
  (fun (cons 'f ())))
