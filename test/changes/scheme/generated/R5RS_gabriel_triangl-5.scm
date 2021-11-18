; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((*board* (vector 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1))
         (*sequence* (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
         (*a* (vector 1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6 6))
         (*b* (vector 2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 5))
         (*c* (vector 4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6 1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4))
         (*answer* ())
         (attempt (lambda (i depth)
                    @sensitivity:FA
                    (if (= depth 14)
                       (begin
                          (set! *answer* (cons (cdr (vector->list *sequence*)) *answer*))
                          #t)
                       (if (if (= 1 (vector-ref *board* (vector-ref *a* i))) (if (= 1 (vector-ref *board* (vector-ref *b* i))) (= 0 (vector-ref *board* (vector-ref *c* i))) #f) #f)
                          (begin
                             (vector-set! *board* (vector-ref *a* i) 0)
                             (vector-set! *board* (vector-ref *b* i) 0)
                             (vector-set! *board* (vector-ref *c* i) 1)
                             (vector-set! *sequence* depth i)
                             (letrec ((__do_loop (lambda (j depth)
                                                   @sensitivity:FA
                                                   (if (let ((__or_res (= j 36))) (if __or_res __or_res (attempt j depth)))
                                                      #f
                                                      (__do_loop (+ j 1) depth)))))
                                (__do_loop 0 (+ depth 1)))
                             (vector-set! *board* (vector-ref *a* i) 1)
                             (vector-set! *board* (vector-ref *b* i) 1)
                             (vector-set! *board* (vector-ref *c* i) 0)
                             #f)
                          #f))))
         (test (lambda (i depth)
                 @sensitivity:FA
                 (set! *answer* ())
                 (attempt i depth)
                 (car *answer*))))
   (<change>
      ()
      6)
   (equal?
      (test 22 1)
      (__toplevel_cons
         22
         (__toplevel_cons
            34
            (__toplevel_cons
               31
               (__toplevel_cons
                  15
                  (__toplevel_cons
                     7
                     (__toplevel_cons
                        1
                        (__toplevel_cons
                           20
                           (__toplevel_cons
                              17
                              (__toplevel_cons
                                 25
                                 (__toplevel_cons 6 (__toplevel_cons 5 (__toplevel_cons 13 (__toplevel_cons 32 ())))))))))))))))