; Changes:
; * removed: 0
; * added: 5
; * swaps: 0
; * negated predicates: 6
; * swapped branches: 4
; * calls to id fun: 6
(letrec ((tak0 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak1 (tak37 (- x 1) y z) (tak11 (- y 1) z x) (tak17 (- z 1) x y)))))
         (tak1 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak2 (tak74 (- x 1) y z) (tak22 (- y 1) z x) (tak34 (- z 1) x y)))))
         (tak2 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak3 (tak11 (- x 1) y z) (tak33 (- y 1) z x) (tak51 (- z 1) x y)))))
         (tak3 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak4 (tak48 (- x 1) y z) (tak44 (- y 1) z x) (tak68 (- z 1) x y)))))
         (tak4 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak5 (tak85 (- x 1) y z) (tak55 (- y 1) z x) (tak85 (- z 1) x y)))))
         (tak5 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak6 (tak22 (- x 1) y z) (tak66 (- y 1) z x) (tak2 (- z 1) x y)))))
         (tak6 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak7 (tak59 (- x 1) y z) (tak77 (- y 1) z x) (tak19 (- z 1) x y)))))
         (tak7 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak8 (tak96 (- x 1) y z) (tak88 (- y 1) z x) (tak36 (- z 1) x y)))))
         (tak8 (lambda (x y z)
                 (if (<change> (not (< y x)) (not (not (< y x))))
                    z
                    (tak9 (tak33 (- x 1) y z) (tak99 (- y 1) z x) (tak53 (- z 1) x y)))))
         (tak9 (lambda (x y z)
                 (if (not (< y x))
                    z
                    (tak10 (tak70 (- x 1) y z) (tak10 (- y 1) z x) (tak70 (- z 1) x y)))))
         (tak10 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak11 (tak7 (- x 1) y z) (tak21 (- y 1) z x) (tak87 (- z 1) x y)))))
         (tak11 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak12 (tak44 (- x 1) y z) (tak32 (- y 1) z x) (tak4 (- z 1) x y)))))
         (tak12 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak13 (tak81 (- x 1) y z) (tak43 (- y 1) z x) (tak21 (- z 1) x y)))))
         (tak13 (lambda (x y z)
                  (<change>
                     ()
                     x)
                  (if (not (< y x))
                     z
                     (tak14 (tak18 (- x 1) y z) (tak54 (- y 1) z x) (tak38 (- z 1) x y)))))
         (tak14 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak15 (tak55 (- x 1) y z) (tak65 (- y 1) z x) (tak55 (- z 1) x y)))))
         (tak15 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak16 (tak92 (- x 1) y z) (tak76 (- y 1) z x) (tak72 (- z 1) x y)))))
         (tak16 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak17 (tak29 (- x 1) y z) (tak87 (- y 1) z x) (tak89 (- z 1) x y)))))
         (tak17 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak18 (tak66 (- x 1) y z) (tak98 (- y 1) z x) (tak6 (- z 1) x y)))))
         (tak18 (lambda (x y z)
                  (if (<change> (not (< y x)) (not (not (< y x))))
                     z
                     (tak19 (tak3 (- x 1) y z) (tak9 (- y 1) z x) (tak23 (- z 1) x y)))))
         (tak19 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak20 (tak40 (- x 1) y z) (tak20 (- y 1) z x) (tak40 (- z 1) x y)))))
         (tak20 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak21 (tak77 (- x 1) y z) (tak31 (- y 1) z x) (tak57 (- z 1) x y)))))
         (tak21 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak22 (tak14 (- x 1) y z) (tak42 (- y 1) z x) (tak74 (- z 1) x y)))))
         (tak22 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak23 (tak51 (- x 1) y z) (tak53 (- y 1) z x) (tak91 (- z 1) x y)))))
         (tak23 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak24 (tak88 (- x 1) y z) (tak64 (- y 1) z x) (tak8 (- z 1) x y)))))
         (tak24 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak25 (tak25 (- x 1) y z) (tak75 (- y 1) z x) (tak25 (- z 1) x y)))))
         (tak25 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak26 (tak62 (- x 1) y z) (tak86 (- y 1) z x) (tak42 (- z 1) x y)))))
         (tak26 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak27 (tak99 (- x 1) y z) (tak97 (- y 1) z x) (tak59 (- z 1) x y)))))
         (tak27 (lambda (x y z)
                  (if (not (< y x))
                     (<change>
                        z
                        (tak28 (tak36 (- x 1) y z) (tak8 (- y 1) z x) (tak76 (- z 1) x y)))
                     (<change>
                        (tak28 (tak36 (- x 1) y z) (tak8 (- y 1) z x) (tak76 (- z 1) x y))
                        z))))
         (tak28 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak29 (tak73 (- x 1) y z) (tak19 (- y 1) z x) (tak93 (- z 1) x y)))))
         (tak29 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak30 (tak10 (- x 1) y z) (tak30 (- y 1) z x) (tak10 (- z 1) x y)))))
         (tak30 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak31 (tak47 (- x 1) y z) (tak41 (- y 1) z x) (tak27 (- z 1) x y)))))
         (tak31 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak32 (tak84 (- x 1) y z) (tak52 (- y 1) z x) (tak44 (- z 1) x y)))))
         (tak32 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak33 (tak21 (- x 1) y z) (tak63 (- y 1) z x) (tak61 (- z 1) x y)))))
         (tak33 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak34 (tak58 (- x 1) y z) (tak74 (- y 1) z x) (tak78 (- z 1) x y)))))
         (tak34 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak35 (tak95 (- x 1) y z) (tak85 (- y 1) z x) (tak95 (- z 1) x y)))))
         (tak35 (lambda (x y z)
                  (<change>
                     (if (not (< y x))
                        z
                        (tak36 (tak32 (- x 1) y z) (tak96 (- y 1) z x) (tak12 (- z 1) x y)))
                     ((lambda (x) x)
                        (if (not (< y x))
                           z
                           (tak36 (tak32 (- x 1) y z) (tak96 (- y 1) z x) (tak12 (- z 1) x y)))))))
         (tak36 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak37 (tak69 (- x 1) y z) (tak7 (- y 1) z x) (tak29 (- z 1) x y)))))
         (tak37 (lambda (x y z)
                  (if (<change> (not (< y x)) (not (not (< y x))))
                     z
                     (tak38 (tak6 (- x 1) y z) (tak18 (- y 1) z x) (tak46 (- z 1) x y)))))
         (tak38 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak39 (tak43 (- x 1) y z) (tak29 (- y 1) z x) (tak63 (- z 1) x y)))))
         (tak39 (lambda (x y z)
                  (<change>
                     ()
                     (display y))
                  (if (not (< y x))
                     z
                     (tak40 (tak80 (- x 1) y z) (tak40 (- y 1) z x) (tak80 (- z 1) x y)))))
         (tak40 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak41 (tak17 (- x 1) y z) (tak51 (- y 1) z x) (tak97 (- z 1) x y)))))
         (tak41 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak42 (tak54 (- x 1) y z) (tak62 (- y 1) z x) (tak14 (- z 1) x y)))))
         (tak42 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak43 (tak91 (- x 1) y z) (tak73 (- y 1) z x) (tak31 (- z 1) x y)))))
         (tak43 (lambda (x y z)
                  (<change>
                     ()
                     -)
                  (if (not (< y x))
                     z
                     (tak44 (tak28 (- x 1) y z) (tak84 (- y 1) z x) (tak48 (- z 1) x y)))))
         (tak44 (lambda (x y z)
                  (<change>
                     (if (not (< y x))
                        z
                        (tak45 (tak65 (- x 1) y z) (tak95 (- y 1) z x) (tak65 (- z 1) x y)))
                     ((lambda (x) x)
                        (if (not (< y x))
                           z
                           (tak45 (tak65 (- x 1) y z) (tak95 (- y 1) z x) (tak65 (- z 1) x y)))))))
         (tak45 (lambda (x y z)
                  (if (not (< y x))
                     (<change>
                        z
                        (tak46 (tak2 (- x 1) y z) (tak6 (- y 1) z x) (tak82 (- z 1) x y)))
                     (<change>
                        (tak46 (tak2 (- x 1) y z) (tak6 (- y 1) z x) (tak82 (- z 1) x y))
                        z))))
         (tak46 (lambda (x y z)
                  (<change>
                     (if (not (< y x))
                        z
                        (tak47 (tak39 (- x 1) y z) (tak17 (- y 1) z x) (tak99 (- z 1) x y)))
                     ((lambda (x) x)
                        (if (not (< y x))
                           z
                           (tak47 (tak39 (- x 1) y z) (tak17 (- y 1) z x) (tak99 (- z 1) x y)))))))
         (tak47 (lambda (x y z)
                  (<change>
                     (if (not (< y x))
                        z
                        (tak48 (tak76 (- x 1) y z) (tak28 (- y 1) z x) (tak16 (- z 1) x y)))
                     ((lambda (x) x)
                        (if (not (< y x))
                           z
                           (tak48 (tak76 (- x 1) y z) (tak28 (- y 1) z x) (tak16 (- z 1) x y)))))))
         (tak48 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak49 (tak13 (- x 1) y z) (tak39 (- y 1) z x) (tak33 (- z 1) x y)))))
         (tak49 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak50 (tak50 (- x 1) y z) (tak50 (- y 1) z x) (tak50 (- z 1) x y)))))
         (tak50 (lambda (x y z)
                  (if (<change> (not (< y x)) (not (not (< y x))))
                     z
                     (tak51 (tak87 (- x 1) y z) (tak61 (- y 1) z x) (tak67 (- z 1) x y)))))
         (tak51 (lambda (x y z)
                  (if (not (< y x))
                     (<change>
                        z
                        (tak52 (tak24 (- x 1) y z) (tak72 (- y 1) z x) (tak84 (- z 1) x y)))
                     (<change>
                        (tak52 (tak24 (- x 1) y z) (tak72 (- y 1) z x) (tak84 (- z 1) x y))
                        z))))
         (tak52 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak53 (tak61 (- x 1) y z) (tak83 (- y 1) z x) (tak1 (- z 1) x y)))))
         (tak53 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak54 (tak98 (- x 1) y z) (tak94 (- y 1) z x) (tak18 (- z 1) x y)))))
         (tak54 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak55 (tak35 (- x 1) y z) (tak5 (- y 1) z x) (tak35 (- z 1) x y)))))
         (tak55 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak56 (tak72 (- x 1) y z) (tak16 (- y 1) z x) (tak52 (- z 1) x y)))))
         (tak56 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak57 (tak9 (- x 1) y z) (tak27 (- y 1) z x) (tak69 (- z 1) x y)))))
         (tak57 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak58 (tak46 (- x 1) y z) (tak38 (- y 1) z x) (tak86 (- z 1) x y)))))
         (tak58 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak59 (tak83 (- x 1) y z) (tak49 (- y 1) z x) (tak3 (- z 1) x y)))))
         (tak59 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak60 (tak20 (- x 1) y z) (tak60 (- y 1) z x) (tak20 (- z 1) x y)))))
         (tak60 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak61 (tak57 (- x 1) y z) (tak71 (- y 1) z x) (tak37 (- z 1) x y)))))
         (tak61 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak62 (tak94 (- x 1) y z) (tak82 (- y 1) z x) (tak54 (- z 1) x y)))))
         (tak62 (lambda (x y z)
                  (<change>
                     ()
                     (display tak93))
                  (if (not (< y x))
                     z
                     (tak63 (tak31 (- x 1) y z) (tak93 (- y 1) z x) (tak71 (- z 1) x y)))))
         (tak63 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak64 (tak68 (- x 1) y z) (tak4 (- y 1) z x) (tak88 (- z 1) x y)))))
         (tak64 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak65 (tak5 (- x 1) y z) (tak15 (- y 1) z x) (tak5 (- z 1) x y)))))
         (tak65 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak66 (tak42 (- x 1) y z) (tak26 (- y 1) z x) (tak22 (- z 1) x y)))))
         (tak66 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak67 (tak79 (- x 1) y z) (tak37 (- y 1) z x) (tak39 (- z 1) x y)))))
         (tak67 (lambda (x y z)
                  (<change>
                     ()
                     -)
                  (if (not (< y x))
                     z
                     (tak68 (tak16 (- x 1) y z) (tak48 (- y 1) z x) (tak56 (- z 1) x y)))))
         (tak68 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak69 (tak53 (- x 1) y z) (tak59 (- y 1) z x) (tak73 (- z 1) x y)))))
         (tak69 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak70 (tak90 (- x 1) y z) (tak70 (- y 1) z x) (tak90 (- z 1) x y)))))
         (tak70 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak71 (tak27 (- x 1) y z) (tak81 (- y 1) z x) (tak7 (- z 1) x y)))))
         (tak71 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak72 (tak64 (- x 1) y z) (tak92 (- y 1) z x) (tak24 (- z 1) x y)))))
         (tak72 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak73 (tak1 (- x 1) y z) (tak3 (- y 1) z x) (tak41 (- z 1) x y)))))
         (tak73 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak74 (tak38 (- x 1) y z) (tak14 (- y 1) z x) (tak58 (- z 1) x y)))))
         (tak74 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak75 (tak75 (- x 1) y z) (tak25 (- y 1) z x) (tak75 (- z 1) x y)))))
         (tak75 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak76 (tak12 (- x 1) y z) (tak36 (- y 1) z x) (tak92 (- z 1) x y)))))
         (tak76 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak77 (tak49 (- x 1) y z) (tak47 (- y 1) z x) (tak9 (- z 1) x y)))))
         (tak77 (lambda (x y z)
                  (if (<change> (not (< y x)) (not (not (< y x))))
                     z
                     (tak78 (tak86 (- x 1) y z) (tak58 (- y 1) z x) (tak26 (- z 1) x y)))))
         (tak78 (lambda (x y z)
                  (<change>
                     (if (not (< y x))
                        z
                        (tak79 (tak23 (- x 1) y z) (tak69 (- y 1) z x) (tak43 (- z 1) x y)))
                     ((lambda (x) x)
                        (if (not (< y x))
                           z
                           (tak79 (tak23 (- x 1) y z) (tak69 (- y 1) z x) (tak43 (- z 1) x y)))))))
         (tak79 (lambda (x y z)
                  (<change>
                     (if (not (< y x))
                        z
                        (tak80 (tak60 (- x 1) y z) (tak80 (- y 1) z x) (tak60 (- z 1) x y)))
                     ((lambda (x) x)
                        (if (not (< y x))
                           z
                           (tak80 (tak60 (- x 1) y z) (tak80 (- y 1) z x) (tak60 (- z 1) x y)))))))
         (tak80 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak81 (tak97 (- x 1) y z) (tak91 (- y 1) z x) (tak77 (- z 1) x y)))))
         (tak81 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak82 (tak34 (- x 1) y z) (tak2 (- y 1) z x) (tak94 (- z 1) x y)))))
         (tak82 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak83 (tak71 (- x 1) y z) (tak13 (- y 1) z x) (tak11 (- z 1) x y)))))
         (tak83 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak84 (tak8 (- x 1) y z) (tak24 (- y 1) z x) (tak28 (- z 1) x y)))))
         (tak84 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak85 (tak45 (- x 1) y z) (tak35 (- y 1) z x) (tak45 (- z 1) x y)))))
         (tak85 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak86 (tak82 (- x 1) y z) (tak46 (- y 1) z x) (tak62 (- z 1) x y)))))
         (tak86 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak87 (tak19 (- x 1) y z) (tak57 (- y 1) z x) (tak79 (- z 1) x y)))))
         (tak87 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak88 (tak56 (- x 1) y z) (tak68 (- y 1) z x) (tak96 (- z 1) x y)))))
         (tak88 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak89 (tak93 (- x 1) y z) (tak79 (- y 1) z x) (tak13 (- z 1) x y)))))
         (tak89 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak90 (tak30 (- x 1) y z) (tak90 (- y 1) z x) (tak30 (- z 1) x y)))))
         (tak90 (lambda (x y z)
                  (if (<change> (not (< y x)) (not (not (< y x))))
                     z
                     (tak91 (tak67 (- x 1) y z) (tak1 (- y 1) z x) (tak47 (- z 1) x y)))))
         (tak91 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak92 (tak4 (- x 1) y z) (tak12 (- y 1) z x) (tak64 (- z 1) x y)))))
         (tak92 (lambda (x y z)
                  (if (not (< y x))
                     (<change>
                        z
                        (tak93 (tak41 (- x 1) y z) (tak23 (- y 1) z x) (tak81 (- z 1) x y)))
                     (<change>
                        (tak93 (tak41 (- x 1) y z) (tak23 (- y 1) z x) (tak81 (- z 1) x y))
                        z))))
         (tak93 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak94 (tak78 (- x 1) y z) (tak34 (- y 1) z x) (tak98 (- z 1) x y)))))
         (tak94 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak95 (tak15 (- x 1) y z) (tak45 (- y 1) z x) (tak15 (- z 1) x y)))))
         (tak95 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak96 (tak52 (- x 1) y z) (tak56 (- y 1) z x) (tak32 (- z 1) x y)))))
         (tak96 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak97 (tak89 (- x 1) y z) (tak67 (- y 1) z x) (tak49 (- z 1) x y)))))
         (tak97 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak98 (tak26 (- x 1) y z) (tak78 (- y 1) z x) (tak66 (- z 1) x y)))))
         (tak98 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak99 (tak63 (- x 1) y z) (tak89 (- y 1) z x) (tak83 (- z 1) x y)))))
         (tak99 (lambda (x y z)
                  (if (not (< y x))
                     z
                     (tak0 (tak0 (- x 1) y z) (tak0 (- y 1) z x) (tak0 (- z 1) x y))))))
   (tak0 18 12 6))