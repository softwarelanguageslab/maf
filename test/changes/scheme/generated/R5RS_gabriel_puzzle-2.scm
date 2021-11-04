; Changes:
; * removed: 5
; * added: 10
; * swaps: 6
; * negated predicates: 3
; * swapped branches: 0
; * calls to id fun: 5
(letrec ((my-iota (lambda (n)
                    @sensitivity:FA
                    (letrec ((__do_loop (lambda (n list)
                                          @sensitivity:FA
                                          (if (zero? n)
                                             list
                                             (__do_loop (- n 1) (cons (- n 1) list))))))
                       (__do_loop n ()))))
         (size 511)
         (classmax 3)
         (typemax 12)
         (*iii* 0)
         (*kount* 0)
         (*d* 8)
         (*piececount* (make-vector (+ classmax 1) 0))
         (*class* (make-vector (+ typemax 1) 0))
         (*piecemax* (make-vector (+ typemax 1) 0))
         (*puzzle* (make-vector (+ size 1)))
         (*p* (make-vector (+ typemax 1)))
         (fit (lambda (i j)
                @sensitivity:FA
                (let ((end (vector-ref *piecemax* i)))
                   (letrec ((__do_loop (lambda (k)
                                         @sensitivity:No
                                         (if (let ((__or_res (> k end))) (if __or_res __or_res (if (vector-ref (vector-ref *p* i) k) (vector-ref *puzzle* (+ j k)) #f)))
                                            (if (> k end) #t #f)
                                            (__do_loop (+ k 1))))))
                      (__do_loop 0)))))
         (place (lambda (i j)
                  @sensitivity:FA
                  (let ((end (vector-ref *piecemax* i)))
                     (letrec ((__do_loop (lambda (k)
                                           @sensitivity:FA
                                           (if (> k end)
                                              #f
                                              (begin
                                                 (if (vector-ref (vector-ref *p* i) k)
                                                    (begin
                                                       (vector-set! *puzzle* (+ j k) #t)
                                                       #t)
                                                    #f)
                                                 (__do_loop (+ k 1)))))))
                        (__do_loop 0))
                     (vector-set!
                        *piececount*
                        (vector-ref *class* i)
                        (- (vector-ref *piececount* (vector-ref *class* i)) 1))
                     (letrec ((__do_loop (lambda (k)
                                           @sensitivity:FA
                                           (if (let ((__or_res (> k size))) (if __or_res __or_res (not (vector-ref *puzzle* k))))
                                              (if (> k size) 0 k)
                                              (__do_loop (+ k 1))))))
                        (__do_loop j)))))
         (puzzle-remove (lambda (i j)
                          (<change>
                             ()
                             *piecemax*)
                          (<change>
                             ()
                             (display *piececount*))
                          @sensitivity:FA
                          (let ((end (vector-ref *piecemax* i)))
                             (<change>
                                ()
                                >)
                             (letrec ((__do_loop (lambda (k)
                                                   (<change>
                                                      @sensitivity:FA
                                                      ())
                                                   (if (> k end)
                                                      #f
                                                      (begin
                                                         (if (vector-ref (vector-ref *p* i) k)
                                                            (begin
                                                               (vector-set! *puzzle* (+ j k) #f)
                                                               #f)
                                                            #f)
                                                         (__do_loop (+ k 1)))))))
                                (__do_loop 0))
                             (vector-set!
                                *piececount*
                                (vector-ref *class* i)
                                (+ (vector-ref *piececount* (vector-ref *class* i)) 1)))))
         (trial (lambda (j)
                  (<change>
                     @sensitivity:FA
                     (let ((k 0)
                           (return #f))
                        (lambda (return)
                           (letrec ((__do_loop (lambda (i)
                                                 @sensitivity:FA
                                                 (if (let ((__or_res return)) (if __or_res __or_res (> i typemax)))
                                                    (begin
                                                       (set! *kount* (+ *kount* 1))
                                                       return)
                                                    (begin
                                                       (if (not (zero? (vector-ref *piececount* (vector-ref *class* i))))
                                                          (if (fit i j)
                                                             (begin
                                                                (set! k (place i j))
                                                                (if (let ((__or_res (trial k))) (if __or_res __or_res (zero? k)))
                                                                   (begin
                                                                      (set! *kount* (+ *kount* 1))
                                                                      (set! return #t))
                                                                   (puzzle-remove i j)))
                                                             #f)
                                                          #f)
                                                       (__do_loop (+ i 1)))))))
                              (__do_loop 0)))))
                  (<change>
                     (let ((k 0)
                           (return #f))
                        (lambda (return)
                           (letrec ((__do_loop (lambda (i)
                                                 @sensitivity:FA
                                                 (if (let ((__or_res return)) (if __or_res __or_res (> i typemax)))
                                                    (begin
                                                       (set! *kount* (+ *kount* 1))
                                                       return)
                                                    (begin
                                                       (if (not (zero? (vector-ref *piececount* (vector-ref *class* i))))
                                                          (if (fit i j)
                                                             (begin
                                                                (set! k (place i j))
                                                                (if (let ((__or_res (trial k))) (if __or_res __or_res (zero? k)))
                                                                   (begin
                                                                      (set! *kount* (+ *kount* 1))
                                                                      (set! return #t))
                                                                   (puzzle-remove i j)))
                                                             #f)
                                                          #f)
                                                       (__do_loop (+ i 1)))))))
                              (__do_loop 0))))
                     @sensitivity:FA)))
         (definePiece (lambda (iclass ii jj kk)
                        (<change>
                           @sensitivity:FA
                           (let ((index 0))
                              (letrec ((__do_loop (lambda (i)
                                                    @sensitivity:FA
                                                    (if (not (> i ii))
                                                       #f
                                                       (begin
                                                          (__do_loop (+ i 1))
                                                          (letrec ((__do_loop (lambda (j)
                                                                                @sensitivity:FA
                                                                                (if (> j jj)
                                                                                   #f
                                                                                   (begin
                                                                                      (letrec ((__do_loop (lambda (k)
                                                                                                            @sensitivity:FA
                                                                                                            (if (> k kk)
                                                                                                               #f
                                                                                                               (begin
                                                                                                                  (set! index (+ i (* *d* (+ j (* *d* k)))))
                                                                                                                  (vector-set! (vector-ref *p* *iii*) index #t)
                                                                                                                  (__do_loop (+ k 1)))))))
                                                                                         (__do_loop 0))
                                                                                      (__do_loop (+ j 1)))))))
                                                             (__do_loop 0)))))))
                                 (__do_loop 0))
                              (vector-set! *piecemax* *iii* index)
                              (if (not (= *iii* typemax))
                                 (set! *iii* (+ *iii* 1))
                                 #f)))
                        (<change>
                           (let ((index 0))
                              (letrec ((__do_loop (lambda (i)
                                                    @sensitivity:FA
                                                    (if (> i ii)
                                                       #f
                                                       (begin
                                                          (letrec ((__do_loop (lambda (j)
                                                                                @sensitivity:FA
                                                                                (if (> j jj)
                                                                                   #f
                                                                                   (begin
                                                                                      (letrec ((__do_loop (lambda (k)
                                                                                                            @sensitivity:FA
                                                                                                            (if (> k kk)
                                                                                                               #f
                                                                                                               (begin
                                                                                                                  (set! index (+ i (* *d* (+ j (* *d* k)))))
                                                                                                                  (vector-set! (vector-ref *p* *iii*) index #t)
                                                                                                                  (__do_loop (+ k 1)))))))
                                                                                         (__do_loop 0))
                                                                                      (__do_loop (+ j 1)))))))
                                                             (__do_loop 0))
                                                          (__do_loop (+ i 1)))))))
                                 (__do_loop 0))
                              (vector-set! *class* *iii* iclass)
                              (vector-set! *piecemax* *iii* index)
                              (if (not (= *iii* typemax))
                                 (set! *iii* (+ *iii* 1))
                                 #f))
                           @sensitivity:FA)))
         (start (lambda ()
                  (<change>
                     ()
                     (display 1))
                  (<change>
                     @sensitivity:FA
                     ((lambda (x) x) @sensitivity:FA))
                  (set! *kount* 0)
                  (letrec ((__do_loop (lambda (m)
                                        @sensitivity:FA
                                        (<change>
                                           (if (> m size)
                                              #f
                                              (begin
                                                 (vector-set! *puzzle* m #t)
                                                 (__do_loop (+ m 1))))
                                           ((lambda (x) x)
                                              (if (> m size)
                                                 #f
                                                 (begin
                                                    (<change>
                                                       (vector-set! *puzzle* m #t)
                                                       ((lambda (x) x) (vector-set! *puzzle* m #t)))
                                                    (__do_loop (+ m 1)))))))))
                     (__do_loop 0))
                  (letrec ((__do_loop (lambda (i)
                                        @sensitivity:FA
                                        (if (> i 5)
                                           #f
                                           (begin
                                              (letrec ((__do_loop (lambda (j)
                                                                    (<change>
                                                                       @sensitivity:FA
                                                                       ())
                                                                    (<change>
                                                                       ()
                                                                       i)
                                                                    (if (> j 5)
                                                                       #f
                                                                       (begin
                                                                          (letrec ((__do_loop (lambda (k)
                                                                                                @sensitivity:FA
                                                                                                (if (<change> (> k 5) (not (> k 5)))
                                                                                                   #f
                                                                                                   (begin
                                                                                                      (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f)
                                                                                                      (__do_loop (+ k 1)))))))
                                                                             (<change>
                                                                                ()
                                                                                __do_loop)
                                                                             (__do_loop 1))
                                                                          (__do_loop (+ j 1)))))))
                                                 (__do_loop 1))
                                              (__do_loop (+ i 1)))))))
                     (__do_loop 1))
                  (letrec ((__do_loop (lambda (i)
                                        @sensitivity:FA
                                        (if (> i typemax)
                                           #f
                                           (begin
                                              (letrec ((__do_loop (lambda (m)
                                                                    (<change>
                                                                       ()
                                                                       #f)
                                                                    @sensitivity:FA
                                                                    (if (<change> (> m size) (not (> m size)))
                                                                       #f
                                                                       (begin
                                                                          (vector-set! (vector-ref *p* i) m #f)
                                                                          (__do_loop (+ m 1)))))))
                                                 (<change>
                                                    (__do_loop 0)
                                                    ((lambda (x) x) (__do_loop 0))))
                                              (<change>
                                                 (__do_loop (+ i 1))
                                                 ((lambda (x) x) (__do_loop (+ i 1)))))))))
                     (<change>
                        ()
                        __do_loop)
                     (__do_loop 0))
                  (set! *iii* 0)
                  (<change>
                     ()
                     (display 2))
                  (definePiece 0 3 1 0)
                  (definePiece 0 1 0 3)
                  (definePiece 0 0 3 1)
                  (<change>
                     (definePiece 0 1 3 0)
                     (definePiece 0 3 0 1))
                  (<change>
                     (definePiece 0 3 0 1)
                     (definePiece 0 1 3 0))
                  (definePiece 0 0 1 3)
                  (definePiece 1 2 0 0)
                  (<change>
                     ()
                     @sensitivity:FA)
                  (definePiece 1 0 2 0)
                  (definePiece 1 0 0 2)
                  (<change>
                     (definePiece 2 1 1 0)
                     (definePiece 2 1 0 1))
                  (<change>
                     (definePiece 2 1 0 1)
                     (definePiece 2 1 1 0))
                  (definePiece 2 0 1 1)
                  (definePiece 3 1 1 1)
                  (vector-set! *piececount* 0 13)
                  (<change>
                     (vector-set! *piececount* 1 3)
                     ())
                  (<change>
                     (vector-set! *piececount* 2 1)
                     (vector-set! *piececount* 3 1))
                  (<change>
                     (vector-set! *piececount* 3 1)
                     (vector-set! *piececount* 2 1))
                  (let ((m (+ (* *d* (+ *d* 1)) 1))
                        (n 0))
                     (if (fit 0 m)
                        (set! n (place 0 m))
                        (begin
                           (<change>
                              (newline)
                              ())
                           (display "Error.")))
                     (if (trial n) *kount* #f)))))
   (for-each
      (lambda (i)
         @sensitivity:FA
         (vector-set! *p* i (make-vector (+ size 1))))
      (my-iota (+ typemax 1)))
   (= (start) 2005))