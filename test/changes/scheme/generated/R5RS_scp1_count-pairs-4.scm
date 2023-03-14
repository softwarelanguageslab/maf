; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((count-pairs (lambda (x)
                        (if (not (pair? x))
                           0
                           (+ (count-pairs (car x)) (count-pairs (cdr x)) 1))))
         (ret3 (cons 'a (cons 'b (cons 'c ()))))
         (ret4 (let ((last (cons 'c ())))
                 (cons last (cons 'b last))))
         (ret7 (let* ((last (cons 'c ()))
                     (middle (cons last last)))
                 (<change>
                    (cons middle middle)
                    ((lambda (x) x) (cons middle middle))))))
   (<change>
      ()
      3)
   (if (= (count-pairs ret3) 3)
      (if (= (count-pairs ret4) 4)
         (= (count-pairs ret7) 7)
         #f)
      #f))