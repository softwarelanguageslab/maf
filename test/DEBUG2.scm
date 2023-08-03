;"test/changes/scheme/generated/R5RS_gabriel_cpstak-5.scm" reduced
(letrec ((tak (lambda (k)
                (if (< 1 1) (k ()) (tak ())))))
  (tak (lambda (a) (<change> () #f))))
