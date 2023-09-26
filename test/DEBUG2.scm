(letrec ((fill-vector-iter! (lambda ()
                              (<change>
                                ()
                                (fill-vector-iter!))
                              ())))
  (fill-vector-iter!))