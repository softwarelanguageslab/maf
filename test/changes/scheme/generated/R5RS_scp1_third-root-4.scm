; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((derde-machtswortel (lambda (x)
                               (letrec ((epsilon 1.000000e-02)
                                        (hulp-derde-machtswortel (lambda (y)
                                                                   (if (< (abs (- (* y y y) x)) epsilon)
                                                                      y
                                                                      (hulp-derde-machtswortel (/ (+ (/ x (* y y)) y y) 3))))))
                                  (<change>
                                     (hulp-derde-machtswortel (/ x 3))
                                     ((lambda (x) x) (hulp-derde-machtswortel (/ x 3))))))))
   (<change>
      ()
      (display 27))
   (= 3.000000e+00 (exact->inexact (derde-machtswortel 27))))