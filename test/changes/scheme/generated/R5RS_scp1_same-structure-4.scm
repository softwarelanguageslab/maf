; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((atom? (lambda (x)
                  (not (pair? x))))
         (same-structure? (lambda (l1 l2)
                            (if (if (atom? l1) (atom? l2) #f)
                               #t
                               (if (let ((__or_res (atom? l1))) (if __or_res __or_res (atom? l2)))
                                  #f
                                  (if (same-structure? (car l1) (car l2))
                                     (same-structure? (cdr l1) (cdr l2))
                                     #f)))))
         (same-structure?-or (lambda (l1 l2)
                               (let ((__or_res (if (atom? l1) (atom? l2) #f)))
                                  (if __or_res
                                     __or_res
                                     (if (pair? l1)
                                        (if (<change> (pair? l2) (not (pair? l2)))
                                           (if (same-structure?-or (car l1) (car l2))
                                              (same-structure?-or (cdr l1) (cdr l2))
                                              #f)
                                           #f)
                                        #f))))))
   (<change>
      ()
      (display __toplevel_cons))
   (<change>
      (if (same-structure? (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 3 4) (__toplevel_cons (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons 6 ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) (__toplevel_cons (__toplevel_cons 9 ()) ())) ())) ())) ())) (__toplevel_cons (__toplevel_cons 'a (__toplevel_cons 'b ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'c 'd) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'e (__toplevel_cons 'f ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'g (__toplevel_cons 'h ())) (__toplevel_cons (__toplevel_cons 'i ()) ())) ())) ())) ())))
         (not
            (same-structure?
               (__toplevel_cons
                  (__toplevel_cons 1 (__toplevel_cons 2 ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons 3 (__toplevel_cons 4 ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              (__toplevel_cons 5 (__toplevel_cons 6 ()))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    (__toplevel_cons 7 (__toplevel_cons 8 ()))
                                    (__toplevel_cons (__toplevel_cons 9 ()) ()))
                                 ()))
                           ()))
                     ()))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons 1 (__toplevel_cons 2 ()))
                        (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           (__toplevel_cons 5 (__toplevel_cons 6 ()))
                           (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                        ()))
                  (__toplevel_cons 9 ()))))
         #f)
      ((lambda (x) x)
         (if (same-structure? (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 3 4) (__toplevel_cons (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons 6 ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) (__toplevel_cons (__toplevel_cons 9 ()) ())) ())) ())) ())) (__toplevel_cons (__toplevel_cons 'a (__toplevel_cons 'b ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'c 'd) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'e (__toplevel_cons 'f ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'g (__toplevel_cons 'h ())) (__toplevel_cons (__toplevel_cons 'i ()) ())) ())) ())) ())))
            (not
               (same-structure?
                  (__toplevel_cons
                     (__toplevel_cons 1 (__toplevel_cons 2 ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           (__toplevel_cons 3 (__toplevel_cons 4 ()))
                           (__toplevel_cons
                              (__toplevel_cons
                                 (__toplevel_cons 5 (__toplevel_cons 6 ()))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       (__toplevel_cons 7 (__toplevel_cons 8 ()))
                                       (__toplevel_cons (__toplevel_cons 9 ()) ()))
                                    ()))
                              ()))
                        ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons
                           (__toplevel_cons 1 (__toplevel_cons 2 ()))
                           (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              (__toplevel_cons 5 (__toplevel_cons 6 ()))
                              (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                           ()))
                     (__toplevel_cons 9 ()))))
            #f))))