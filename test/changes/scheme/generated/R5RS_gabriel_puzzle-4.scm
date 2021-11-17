; Changes:
; * removed: 2
; * added: 11
; * swaps: 7
; * negated predicates: 1
; * swapped branches: 4
; * calls to id fun: 11
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
                                         (<change>
                                            ()
                                            *p*)
                                         @sensitivity:No
                                         (if (let ((__or_res (> k end))) (if __or_res __or_res (if (<change> (vector-ref (vector-ref *p* i) k) (not (vector-ref (vector-ref *p* i) k))) (vector-ref *puzzle* (+ j k)) #f)))
                                            (if (> k end) #t #f)
                                            (__do_loop (+ k 1))))))
                      (<change>
                         (__do_loop 0)
                         ((lambda (x) x) (__do_loop 0)))))))
         (place (lambda (i j)
                  @sensitivity:FA
                  (let ((end (vector-ref *piecemax* i)))
                     (letrec ((__do_loop (lambda (k)
                                           (<change>
                                              @sensitivity:FA
                                              (if (> k end)
                                                 (begin
                                                    (if (vector-ref (vector-ref *p* i) k)
                                                       (begin
                                                          (vector-set! *puzzle* (+ j k) #t)
                                                          ((lambda (x) x) #t))
                                                       #f)
                                                    (__do_loop (+ k 1))
                                                    1
                                                    (__do_loop (+ k 1)))
                                                 #f))
                                           (<change>
                                              (if (> k end)
                                                 #f
                                                 (begin
                                                    (if (vector-ref (vector-ref *p* i) k)
                                                       (begin
                                                          (vector-set! *puzzle* (+ j k) #t)
                                                          #t)
                                                       #f)
                                                    (__do_loop (+ k 1))))
                                              @sensitivity:FA))))
                        (__do_loop 0))
                     (<change>
                        (vector-set!
                           *piececount*
                           (vector-ref *class* i)
                           (- (vector-ref *piececount* (vector-ref *class* i)) 1))
                        ((lambda (x) x)
                           (vector-set!
                              *piececount*
                              (vector-ref *class* i)
                              (- (vector-ref *piececount* (vector-ref *class* i)) 1))))
                     (letrec ((__do_loop (lambda (k)
                                           (<change>
                                              @sensitivity:FA
                                              ())
                                           (if (let ((__or_res (> k size))) (if __or_res __or_res (not (vector-ref *puzzle* k))))
                                              (if (> k size) 0 k)
                                              (__do_loop (+ k 1))))))
                        (__do_loop j)))))
         (puzzle-remove (lambda (i j)
                          @sensitivity:FA
                          (let ((end (vector-ref *piecemax* i)))
                             (letrec ((__do_loop (lambda (k)
                                                   (<change>
                                                      @sensitivity:FA
                                                      (if (> k end)
                                                         #f
                                                         (begin
                                                            ((lambda (x) x)
                                                               (if (vector-ref (vector-ref *p* i) k)
                                                                  #f
                                                                  (begin
                                                                     (vector-set! *puzzle* (+ j k) #f)
                                                                     #f)))
                                                            ((lambda (x) x) (__do_loop (+ k 1))))))
                                                   (<change>
                                                      (if (> k end)
                                                         #f
                                                         (begin
                                                            (if (vector-ref (vector-ref *p* i) k)
                                                               (begin
                                                                  (vector-set! *puzzle* (+ j k) #f)
                                                                  #f)
                                                               #f)
                                                            (__do_loop (+ k 1))))
                                                      @sensitivity:FA))))
                                (<change>
                                   ()
                                   __do_loop)
                                (__do_loop 0))
                             (vector-set!
                                *piececount*
                                (vector-ref *class* i)
                                (+ (vector-ref *piececount* (vector-ref *class* i)) 1)))))
         (trial (lambda (j)
                  @sensitivity:FA
                  (let ((k 0)
                        (return #f))
                     (lambda (return)
                        (<change>
                           ()
                           (display return))
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
                           (__do_loop 0))))))
         (definePiece (lambda (iclass ii jj kk)
                        @sensitivity:FA
                        (let ((index 0))
                           (<change>
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
                              ((lambda (x) x)
                                 (letrec ((__do_loop (lambda (i)
                                                       @sensitivity:FA
                                                       (if (> i ii)
                                                          #f
                                                          (begin
                                                             (letrec ((__do_loop (lambda (j)
                                                                                   (<change>
                                                                                      @sensitivity:FA
                                                                                      ((lambda (x) x) @sensitivity:FA))
                                                                                   (if (> j jj)
                                                                                      #f
                                                                                      (begin
                                                                                         (letrec ((__do_loop (lambda (k)
                                                                                                               @sensitivity:FA
                                                                                                               (if (> k kk)
                                                                                                                  (<change>
                                                                                                                     #f
                                                                                                                     (begin
                                                                                                                        (set! index (+ i (* *d* (+ j (* *d* k)))))
                                                                                                                        (__do_loop (+ k 1))
                                                                                                                        ((lambda (x) x) (vector-set! (vector-ref *p* *iii*) index #t))))
                                                                                                                  (<change>
                                                                                                                     (begin
                                                                                                                        (set! index (+ i (* *d* (+ j (* *d* k)))))
                                                                                                                        (vector-set! (vector-ref *p* *iii*) index #t)
                                                                                                                        (__do_loop (+ k 1)))
                                                                                                                     #f)))))
                                                                                            (__do_loop 0))
                                                                                         (__do_loop (+ j 1)))))))
                                                                (__do_loop 0))
                                                             (__do_loop (+ i 1)))))))
                                    (__do_loop 0))))
                           (<change>
                              ()
                              (display __do_loop))
                           (vector-set! *class* *iii* iclass)
                           (vector-set! *piecemax* *iii* index)
                           (if (not (= *iii* typemax))
                              (set! *iii* (+ *iii* 1))
                              #f))))
         (start (lambda ()
                  @sensitivity:FA
                  (set! *kount* 0)
                  (letrec ((__do_loop (lambda (m)
                                        @sensitivity:FA
                                        (if (> m size)
                                           #f
                                           (begin
                                              (<change>
                                                 ()
                                                 *puzzle*)
                                              (vector-set! *puzzle* m #t)
                                              (__do_loop (+ m 1)))))))
                     (__do_loop 0))
                  (letrec ((__do_loop (lambda (i)
                                        @sensitivity:FA
                                        (if (> i 5)
                                           #f
                                           (begin
                                              (letrec ((__do_loop (lambda (j)
                                                                    @sensitivity:FA
                                                                    (if (> j 5)
                                                                       #f
                                                                       (begin
                                                                          (letrec ((__do_loop (lambda (k)
                                                                                                (<change>
                                                                                                   @sensitivity:FA
                                                                                                   (if (> k 5)
                                                                                                      #f
                                                                                                      (begin
                                                                                                         (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f)
                                                                                                         (__do_loop (+ k 1)))))
                                                                                                (<change>
                                                                                                   (if (> k 5)
                                                                                                      #f
                                                                                                      (begin
                                                                                                         (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f)
                                                                                                         (__do_loop (+ k 1))))
                                                                                                   @sensitivity:FA))))
                                                                             (__do_loop 1))
                                                                          (__do_loop (+ j 1)))))))
                                                 (<change>
                                                    (__do_loop 1)
                                                    ((lambda (x) x) (__do_loop 1))))
                                              (__do_loop (+ i 1)))))))
                     (__do_loop 1))
                  (<change>
                     (letrec ((__do_loop (lambda (i)
                                           @sensitivity:FA
                                           (if (> i typemax)
                                              #f
                                              (begin
                                                 (letrec ((__do_loop (lambda (m)
                                                                       @sensitivity:FA
                                                                       (if (> m size)
                                                                          #f
                                                                          (begin
                                                                             (vector-set! (vector-ref *p* i) m #f)
                                                                             (__do_loop (+ m 1)))))))
                                                    (__do_loop 0))
                                                 (__do_loop (+ i 1)))))))
                        (__do_loop 0))
                     (set! *iii* 0))
                  (<change>
                     (set! *iii* 0)
                     (letrec ((__do_loop (lambda (i)
                                           (display (__do_loop 0))
                                           @sensitivity:FA
                                           (if (> i typemax)
                                              (begin
                                                 (display size)
                                                 (letrec ((__do_loop (lambda (m)
                                                                       (if (> m size)
                                                                          #f
                                                                          (begin
                                                                             (vector-set! (vector-ref *p* i) m #f)
                                                                             (__do_loop (+ m 1))))
                                                                       @sensitivity:FA)))
                                                    0
                                                    (__do_loop 0))
                                                 (__do_loop (+ i 1)))
                                              #f))))
                        (__do_loop 0)))
                  (definePiece 0 3 1 0)
                  (definePiece 0 1 0 3)
                  (<change>
                     ()
                     vector-set!)
                  (definePiece 0 0 3 1)
                  (definePiece 0 1 3 0)
                  (definePiece 0 3 0 1)
                  (definePiece 0 0 1 3)
                  (definePiece 1 2 0 0)
                  (definePiece 1 0 2 0)
                  (definePiece 1 0 0 2)
                  (<change>
                     (definePiece 2 1 1 0)
                     (definePiece 2 1 0 1))
                  (<change>
                     (definePiece 2 1 0 1)
                     (definePiece 2 1 1 0))
                  (definePiece 2 0 1 1)
                  (<change>
                     (definePiece 3 1 1 1)
                     ())
                  (vector-set! *piececount* 0 13)
                  (vector-set! *piececount* 1 3)
                  (vector-set! *piececount* 2 1)
                  (vector-set! *piececount* 3 1)
                  (let ((m (+ (* *d* (+ *d* 1)) 1))
                        (n 0))
                     (if (fit 0 m)
                        (set! n (place 0 m))
                        (begin
                           (newline)
                           (display "Error.")))
                     (if (trial n) *kount* #f)))))
   (for-each
      (lambda (i)
         @sensitivity:FA
         (<change>
            (vector-set! *p* i (make-vector (+ size 1)))
            ((lambda (x) x) (vector-set! *p* i (make-vector (+ size 1))))))
      (my-iota (+ typemax 1)))
   (<change>
      (= (start) 2005)
      ((lambda (x) x) (= (start) 2005))))