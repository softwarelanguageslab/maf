(letrec ((not (lambda (x) (if x #f #t)))
         (phi (lambda (x2) (<change> (not x2) x2))))
   (let ((res (phi #t)))
      (if res res (phi #f))))
