(letrec ((for-each (lambda (f l)
                      (if (null? l)
                        #t
                        (begin
                          (for-each f (cdr l))))))
          (list (lambda args args))
          (_0 (letrec ((loop (lambda (level)
                                (if (< level 2)
                                  (for-each (lambda (child-pt1) (loop (+ 1 level))) (list ((<change> list vector) '<pt>)))
                                  #f))))
                (loop 0))))
  _0)