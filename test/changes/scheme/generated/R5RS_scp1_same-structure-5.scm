; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 2
; * swapped branches: 2
; * calls to id fun: 2
(letrec ((atom? (lambda (x)
                  (not (pair? x))))
         (same-structure? (lambda (l1 l2)
                            (<change>
                               (if (if (atom? l1) (atom? l2) #f)
                                  #t
                                  (if (let ((__or_res (atom? l1))) (if __or_res __or_res (atom? l2)))
                                     #f
                                     (if (same-structure? (car l1) (car l2))
                                        (same-structure? (cdr l1) (cdr l2))
                                        #f)))
                               ((lambda (x) x)
                                  (if (if (atom? l1) (<change> (atom? l2) #f) (<change> #f (atom? l2)))
                                     #t
                                     (if (<change> (let ((__or_res (atom? l1))) ((lambda (x) x) (if __or_res __or_res (atom? l2)))) (not (let ((__or_res (atom? l1))) ((lambda (x) x) (if __or_res __or_res (atom? l2))))))
                                        #f
                                        (if (<change> (same-structure? (car l1) (car l2)) (not (same-structure? (car l1) (car l2))))
                                           (same-structure? (cdr l1) (cdr l2))
                                           #f)))))))
         (same-structure?-or (lambda (l1 l2)
                               (let ((__or_res (if (atom? l1) (atom? l2) #f)))
                                  (if __or_res
                                     __or_res
                                     (if (pair? l1)
                                        (if (pair? l2)
                                           (<change>
                                              (if (same-structure?-or (car l1) (car l2))
                                                 (same-structure?-or (cdr l1) (cdr l2))
                                                 #f)
                                              #f)
                                           (<change>
                                              #f
                                              (if (same-structure?-or (car l1) (car l2))
                                                 (same-structure?-or (cdr l1) (cdr l2))
                                                 #f)))
                                        #f))))))
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
      #f))