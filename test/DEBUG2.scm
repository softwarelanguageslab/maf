(letrec ((f (lambda (x) (<change> #f x))) 
         (_ (if (f #t) () (f #f))))
  ())