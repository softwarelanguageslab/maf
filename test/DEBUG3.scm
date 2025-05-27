; "test/changes/scheme/generated/R5RS_scp1_dedouble-2.scm"
(letrec ((iter (lambda (bool)
                 (if bool
                   (<change>
                     (lambda arg
                       ())
                     ())
                   (iter ())))))
  (iter #f))