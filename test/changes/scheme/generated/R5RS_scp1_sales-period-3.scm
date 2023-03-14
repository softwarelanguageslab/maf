; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 0
(letrec ((foldr (lambda (f base lst)
                  (letrec ((foldr-aux (lambda (lst)
                                        (if (null? lst)
                                           base
                                           (f (car lst) (foldr-aux (cdr lst)))))))
                     (foldr-aux lst))))
         (totaal (lambda (aankopen kortingen)
                   (letrec ((zoek-korting (lambda (kortingen artikel)
                                            (foldr + 0 (map (lambda (x) (if (eq? (car x) artikel) (cadr x) 0)) kortingen)))))
                      (if (null? aankopen)
                         0
                         (let* ((aankoop (car aankopen))
                                (korting (zoek-korting kortingen (car aankoop)))
                                (prijs (cadr aankoop)))
                            (+ (- prijs (/ (* prijs korting) 100)) (totaal (cdr aankopen) (cdr kortingen))))))))
         (totaal-iter (lambda (aankopen kortingen)
                        (letrec ((zoek-korting (lambda (kortingen artikel)
                                                 (<change>
                                                    ()
                                                    (display (foldr + 0 (map (lambda (x) (if (eq? (car x) artikel) 0 (cadr x))) kortingen))))
                                                 (foldr
                                                    +
                                                    0
                                                    (map (lambda (x) (<change> () (display x)) (if (eq? (car x) artikel) (cadr x) 0)) kortingen))))
                                 (loop (lambda (lst res)
                                         (if (null? lst)
                                            res
                                            (let* ((aankoop (car lst))
                                                   (korting (zoek-korting kortingen (car aankoop)))
                                                   (prijs (cadr aankoop)))
                                               (loop (cdr lst) (+ res (- prijs (/ (* prijs korting) 100)))))))))
                           (loop aankopen 0))))
         (Z&Mkortingen (__toplevel_cons
                         (__toplevel_cons 'jas (__toplevel_cons 50 ()))
                         (__toplevel_cons
                            (__toplevel_cons 'kleed (__toplevel_cons 50 ()))
                            (__toplevel_cons
                               (__toplevel_cons 'rok (__toplevel_cons 30 ()))
                               (__toplevel_cons (__toplevel_cons 'trui (__toplevel_cons 20 ())) ()))))))
   (if (= (totaal (__toplevel_cons (__toplevel_cons 'jas (__toplevel_cons 100 ())) (__toplevel_cons (__toplevel_cons 'trui (__toplevel_cons 25 ())) (__toplevel_cons (__toplevel_cons 'rok (__toplevel_cons 70 ())) (__toplevel_cons (__toplevel_cons 't-shirt (__toplevel_cons 20 ())) ())))) (__toplevel_cons (__toplevel_cons 'jas (__toplevel_cons 50 ())) (__toplevel_cons (__toplevel_cons 'kleed (__toplevel_cons 50 ())) (__toplevel_cons (__toplevel_cons 'rok (__toplevel_cons 30 ())) (__toplevel_cons (__toplevel_cons 'trui (__toplevel_cons 20 ())) ()))))) 139)
      (=
         (totaal-iter
            (__toplevel_cons
               (__toplevel_cons 'jas (__toplevel_cons 100 ()))
               (__toplevel_cons
                  (__toplevel_cons 'trui (__toplevel_cons 25 ()))
                  (__toplevel_cons
                     (__toplevel_cons 'rok (__toplevel_cons 70 ()))
                     (__toplevel_cons (__toplevel_cons 't-shirt (__toplevel_cons 20 ())) ()))))
            (__toplevel_cons
               (__toplevel_cons 'jas (__toplevel_cons 50 ()))
               (__toplevel_cons
                  (__toplevel_cons 'kleed (__toplevel_cons 50 ()))
                  (__toplevel_cons
                     (__toplevel_cons 'rok (__toplevel_cons 30 ()))
                     (__toplevel_cons (__toplevel_cons 'trui (__toplevel_cons 20 ())) ())))))
         139)
      #f))