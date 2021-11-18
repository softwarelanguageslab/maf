; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((atom? (lambda (x)
                  (not (pair? x))))
         (depth (lambda (tree)
                  (if (null? tree)
                     0
                     (if (atom? tree)
                        0
                        (max (+ 1 (depth (car tree))) (depth (cdr tree)))))))
         (leaf-count (lambda (tree)
                       (if (null? tree)
                          0
                          (if (atom? tree)
                             1
                             (+ (leaf-count (car tree)) (leaf-count (cdr tree)))))))
         (depth-and-leaf-count (lambda (tree)
                                 (letrec ((make-res cons)
                                          (depth car)
                                          (leaf-count cdr))
                                    (if (null? tree)
                                       (make-res 0 0)
                                       (if (atom? tree)
                                          (make-res 0 1)
                                          (let ((res-car (depth-and-leaf-count (car tree)))
                                                (res-cdr (depth-and-leaf-count (cdr tree))))
                                             (make-res
                                                (max (+ 1 (depth res-car)) (depth res-cdr))
                                                (+ (leaf-count res-car) (leaf-count res-cdr)))))))))
         (l (__toplevel_cons
              (__toplevel_cons 1 (__toplevel_cons 2 ()))
              (__toplevel_cons
                 (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) (__toplevel_cons 5 ()))
                 (__toplevel_cons (__toplevel_cons 6 (__toplevel_cons 7 ())) ())))))
   (if (<change> (= (depth l) 3) (not (= (depth l) 3)))
      (if (= (leaf-count l) 7)
         (equal? (depth-and-leaf-count l) (cons 3 7))
         #f)
      #f))