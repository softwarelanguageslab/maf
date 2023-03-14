; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
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
                          (letrec ((rechthoek (maak-rechthoek zijde zijde))
                                   (schaal! (lambda (n)
                                              (set! zijde (* n zijde))))
                                   (dispatch (lambda (m)
                                               (if (eq? m 'oppervlakte)
                                                  (rechthoek 'oppervlakte)
                                                  (if (eq? m 'omtrek)
                                                     (rechthoek 'omtrek)
                                                     (if (eq? m 'schaal!) schaal! #f))))))
                             dispatch)))
         (test (maak-vierkant 5)))
   (if (<change> (= (test 'oppervlakte) 25) (not (= (test 'oppervlakte) 25)))
      (if (= (test 'omtrek) 20)
         (= (begin ((test 'schaal!) 2) (test 'oppervlakte)) 25)
         #f)
      #f))