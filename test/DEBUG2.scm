;"test/changes/scheme/generated/R5RS_sigscheme_mem-1.scm"
(letrec ((foo (lambda ()
                (<change>
                  (lambda arg
                    ())
                  (foo)))))
  (foo))