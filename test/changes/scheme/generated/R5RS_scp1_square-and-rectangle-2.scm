; Changes:
; * removed: 0
; * added: 4
; * swaps: 0
; * negated predicates: 1
(letrec ((maak-rechthoek (lambda (l b)
                           (letrec ((oppervlakte (lambda ()
                                                   (<change>
                                                      ()
                                                      b)
                                                   (<change>
                                                      ()
                                                      b)
                                                   (* l b)))
                                    (omtrek (lambda ()
                                              (* 2 (+ l b))))
                                    (dispatch (lambda (m)
                                                (if (eq? m 'oppervlakte)
                                                   (oppervlakte)
                                                   (if (<change> (eq? m 'omtrek) (not (eq? m 'omtrek)))
                                                      (omtrek)
                                                      #f)))))
                              dispatch)))
         (maak-vierkant (lambda (zijde)
                          (letrec ((rechthoek (maak-rechthoek zijde zijde))
                                   (schaal! (lambda (n)
                                              (<change>
                                                 ()
                                                 n)
                                              (set! zijde (* n zijde))))
                                   (dispatch (lambda (m)
                                               (if (eq? m 'oppervlakte)
                                                  (rechthoek 'oppervlakte)
                                                  (if (eq? m 'omtrek)
                                                     (rechthoek 'omtrek)
                                                     (if (eq? m 'schaal!) schaal! #f))))))
                             (<change>
                                ()
                                (display dispatch))
                             dispatch)))
         (test (maak-vierkant 5)))
   (if (= (test 'oppervlakte) 25)
      (if (= (test 'omtrek) 20)
         (= (begin ((test 'schaal!) 2) (test 'oppervlakte)) 25)
         #f)
      #f))