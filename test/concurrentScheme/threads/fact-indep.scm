;; Expected result: #t
(letrec ((fact (lambda (n)
                 (if (= n 0)
                     1
                     (* n (fact (- n 1))))))
         (t1 (fork (fact 5)))
         (t2 (fork (fact 4))))
  (= (+ (join t1) (join t2)) 144))
