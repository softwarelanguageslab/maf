(letrec ((count-pairs (lambda (lst)
                        (letrec ((count (lambda (current)
                                          (count current))))
                          (count lst))))
          (retno (<change>
                   ()
                   #f)))
  (count-pairs retno))