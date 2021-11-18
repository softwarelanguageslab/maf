; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((atom? (lambda (x)
                  (<change>
                     (not (pair? x))
                     ((lambda (x) x) (not (pair? x))))))
         (maak-dier (lambda (naam eigenschappen)
                      (list naam eigenschappen)))
         (naam (lambda (dier)
                 (car dier)))
         (eigenschappen (lambda (dier)
                          (cadr dier)))
         (dier? (lambda (dier)
                  (if (pair? dier)
                     (if (atom? (naam dier))
                        (pair? (eigenschappen dier))
                        #f)
                     #f)))
         (maak-boom (lambda (knoop deelbomen)
                      (list knoop deelbomen)))
         (knoop (lambda (boom)
                  (<change>
                     (car boom)
                     ((lambda (x) x) (car boom)))))
         (deelbomen (lambda (boom)
                      (<change>
                         ()
                         boom)
                      (cadr boom)))
         (leeg? (lambda (boom)
                  (null? boom)))
         (knoop? (lambda (boom)
                   (dier? boom)))
         (classificatieboom (maak-boom
                              (maak-dier 'dier (__toplevel_cons 'kan-ademen (__toplevel_cons 'kan-bewegen ())))
                              (list
                                 (maak-boom
                                    (maak-dier
                                       'vis
                                       (__toplevel_cons 'kan-zwemmen (__toplevel_cons 'heeft-schubben (__toplevel_cons 'heeft-vinnen ()))))
                                    (list (maak-dier 'ballonvis (__toplevel_cons 'kan-zwellen (__toplevel_cons 'is-geel ())))))
                                 (maak-boom
                                    (maak-dier
                                       'landdier
                                       (__toplevel_cons 'heeft-huid (__toplevel_cons 'kan-lopen (__toplevel_cons 'heeft-poten ()))))
                                    (list (maak-dier 'olifant (__toplevel_cons 'is-groot ()))))
                                 (maak-boom
                                    (maak-dier
                                       'vogel
                                       (__toplevel_cons 'kan-vliegen (__toplevel_cons 'heeft-vleugels (__toplevel_cons 'heeft-veren ()))))
                                    (list
                                       (maak-dier 'kanarie (__toplevel_cons 'kan-zingen (__toplevel_cons 'is-geel ())))
                                       (maak-dier 'arend (__toplevel_cons 'is-groot ())))))))
         (all-kinds (lambda (boom)
                      (if (leeg? boom)
                         ()
                         (if (dier? boom)
                            (list (naam boom))
                            (if (dier? (knoop boom))
                               (append (list (naam (knoop boom))) (all-kinds-in (deelbomen boom)))
                               (all-kinds-in (deelbomen boom)))))))
         (all-kinds-in (lambda (lst)
                         (if (null? lst)
                            ()
                            (append (all-kinds (car lst)) (all-kinds-in (cdr lst))))))
         (geef-eigenschappen (lambda (boom soort)
                               (<change>
                                  ()
                                  (if (eq? (naam boom) soort)
                                     (append eig (list (eigenschappen boom)))
                                     #f))
                               (letrec ((geef-eig (lambda (boom eig)
                                                    (if (dier? boom)
                                                       (if (eq? (naam boom) soort)
                                                          (append eig (list (eigenschappen boom)))
                                                          #f)
                                                       (if (if (dier? (knoop boom)) (eq? (naam (knoop boom)) soort) #f)
                                                          (append eig (eigenschappen (knoop boom)))
                                                          (geef-eig-in (deelbomen boom) (append eig (eigenschappen (knoop boom))))))))
                                        (geef-eig-in (lambda (lst eig)
                                                       (if (null? lst)
                                                          #f
                                                          (let ((__or_res (geef-eig (car lst) eig)))
                                                             (if __or_res
                                                                __or_res
                                                                (geef-eig-in (cdr lst) eig)))))))
                                  (geef-eig boom ()))))
         (ask? (lambda (boom soort eig)
                 (let ((eigenschappen (geef-eigenschappen boom soort)))
                    (pair? (memq eig eigenschappen))))))
   (if (equal? (all-kinds classificatieboom) (__toplevel_cons 'dier (__toplevel_cons 'vis (__toplevel_cons 'ballonvis (__toplevel_cons 'landdier (__toplevel_cons 'olifant (__toplevel_cons 'vogel (__toplevel_cons 'kanarie (__toplevel_cons 'arend ())))))))))
      (if (ask? classificatieboom 'landdier 'kan-lopen)
         (if (ask? classificatieboom 'ballonvis 'heeft-vinnen)
            (not (ask? classificatieboom 'olifant 'kan-vliegen))
            #f)
         #f)
      #f))