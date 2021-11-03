; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 1
(letrec ((maak-rechthoek (lambda (l b)
                           (letrec ((oppervlakte (lambda ()
                                                   (* l b)))
                                    (omtrek (lambda ()
                                              (* 2 (+ l b))))
                                    (dispatch (lambda (m)
                                                (if (eq? m 'oppervlakte)
                                                   (oppervlakte)
                                                   (if (eq? m 'omtrek) (omtrek) #f)))))
                              dispatch)))
         (maak-vierkant (lambda (zijde)
                          (<change>
                             ()
                             m)
                          (letrec ((rechthoek (maak-rechthoek zijde zijde))
                                   (schaal! (lambda (n)
                                              (set! zijde (* n zijde))))
                                   (dispatch (lambda (m)
                                               (if (eq? m 'oppervlakte)
                                                  (<change>
                                                     (rechthoek 'oppervlakte)
                                                     (if (eq? m 'omtrek)
                                                        (rechthoek 'omtrek)
                                                        (if (eq? m 'schaal!) schaal! #f)))
                                                  (<change>
                                                     (if (eq? m 'omtrek)
                                                        (rechthoek 'omtrek)
                                                        (if (eq? m 'schaal!) schaal! #f))
                                                     (rechthoek 'oppervlakte))))))
                             dispatch)))
         (test (maak-vierkant 5)))
   (<change>
      (if (= (test 'oppervlakte) 25)
         (if (= (test 'omtrek) 20)
            (= (begin ((test 'schaal!) 2) (test 'oppervlakte)) 25)
            #f)
         #f)
      ((lambda (x) x)
         (if (= (test 'oppervlakte) 25)
            (if (= (test 'omtrek) 20)
               (= (begin (<change> () test) ((test 'schaal!) 2) (test 'oppervlakte)) 25)
               #f)
            #f))))