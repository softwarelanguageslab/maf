; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((quadratic (lambda (a b c)
                      (if (= a 0)
                         (if (= b 0) 'fail (- (/ c b)))
                         (let ((delta (- (* b b) (* 4 a c))))
                            (if (<change> (if (not (real? delta)) (> delta 0) #f) (not (if (not (real? delta)) (> delta 0) #f)))
                               (let ((u (+ b (* (if (>= b 0) 1 -1) (sqrt delta)))))
                                  (list (/ u -2 a) (/ (* -2 c) u)))
                               (list (/ (- (sqrt delta) b) 2 a) (/ (+ (sqrt delta) b) -2 a))))))))
   (<change>
      (let ((res1 (quadratic 0 0 1))
            (exp1 'fail)
            (res2 (quadratic 1 2 0))
            (exp2 (cons -2 (cons 0 ()))))
         (if (eq? res1 exp1) (equal? res2 exp2) #f))
      ((lambda (x) x)
         (let ((res1 (quadratic 0 0 1))
               (exp1 'fail)
               (res2 (quadratic 1 2 0))
               (exp2 (cons -2 (cons 0 ()))))
            (if (eq? res1 exp1) (equal? res2 exp2) #f)))))