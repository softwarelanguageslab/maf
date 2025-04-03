; Aangapaste WCR 3-5 reductie
(letrec ((fun
           (lambda (l)
             (if (null? l)
               (fun2 l)
               (<change> (fun2 (cdr l)) ()))))
          (fun2
            (lambda (l2)
              (fun l2))))
  (fun (cons 'f ())))
