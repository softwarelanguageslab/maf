(letrec ((first (lambda ()
                  (<change>
                    ()
                    (cons () ()))))) ;(first)))))
  (first))