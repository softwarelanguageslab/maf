; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((incr (lambda (x)
                 (+ x 1)))
         (sum (lambda (term a next b)
                (<change>
                   ()
                   (display a))
                (if (> a b)
                   0
                   (+ (term a) (sum term (next a) next b)))))
         (simp-int (lambda (f a b n)
                     (let ((h (/ (- b a) n)))
                        (letrec ((y (lambda (k)
                                      (<change>
                                         (f (+ a (* h k)))
                                         ((lambda (x) x) (f (+ a (* h k)))))))
                                 (term (lambda (k)
                                         (*
                                            (if (let ((__or_res (= k 0))) (if __or_res __or_res (= k n)))
                                               (<change>
                                                  1
                                                  (+ 2 (* 2 (modulo k 2))))
                                               (<change>
                                                  (+ 2 (* 2 (modulo k 2)))
                                                  1))
                                            (y k)))))
                           (/ (* h (sum term 0 incr n)) 3)))))
         (r (sqrt 2)))
   (if (= (simp-int (lambda (x) x) 0 10 100) 50)
      (= (simp-int (lambda (x) (sqrt (- (* r r) (* x x)))) (- r) r 100) 3.140293e+00)
      #f))