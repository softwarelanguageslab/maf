; Gereduceerd van "test/changes/scheme/generated/R5RS_various_four-in-a-row-5.scm"
(letrec ((f (lambda ()
              (<change>
                ()
                (f))
              ())))
  (f))