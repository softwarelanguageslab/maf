(letrec ((f (lambda ()
              (<change> () (f))
              ())))
  (f))