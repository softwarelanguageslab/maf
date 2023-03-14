; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((derde-machtswortel (lambda (x)
                               (<change>
                                  ()
                                  *)
                               (letrec ((epsilon 1.000000e-02)
                                        (hulp-derde-machtswortel (lambda (y)
                                                                   (if (< (abs (- (* y y y) x)) epsilon)
                                                                      y
                                                                      (hulp-derde-machtswortel (/ (+ (/ x (* y y)) y y) 3))))))
                                  (hulp-derde-machtswortel (/ x 3))))))
   (= 3.000000e+00 (exact->inexact (derde-machtswortel 27))))