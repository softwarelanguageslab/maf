; Changes:
; * removed: 6
; * added: 5
; * swaps: 7
; * negated predicates: 1
; * swapped branches: 3
; * calls to id fun: 10
(letrec ((my-iota (lambda (n)
                    (<change>
                       @sensitivity:FA
                       ((lambda (x) x) @sensitivity:FA))
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
                                            @sensitivity:No
                                            (if (let ((__or_res (> k end))) (if __or_res __or_res (if (vector-ref (vector-ref *p* i) k) (vector-ref *puzzle* (+ j k)) #f)))
                                               (if (> k end) #t #f)
                                               (__do_loop (+ k 1))))
                                         (<change>
                                            (if (let ((__or_res (> k end))) (if __or_res __or_res (if (vector-ref (vector-ref *p* i) k) (vector-ref *puzzle* (+ j k)) #f)))
                                               (if (> k end) #t #f)
                                               (__do_loop (+ k 1)))
                                            @sensitivity:No))))
                      (__do_loop 0)))))
         (place (lambda (i j)
                  @sensitivity:FA
                  (let ((end (vector-ref *piecemax* i)))
                     (letrec ((__do_loop (lambda (k)
                                           (<change>
                                              @sensitivity:FA
                                              ((lambda (x) x) @sensitivity:FA))
                                           (<change>
                                              ()
                                              k)
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
                     (<change>
                        ()
                        i)
                     (letrec ((__do_loop (lambda (k)
                                           @sensitivity:FA
                                           (if (let ((__or_res (> k size))) (if __or_res __or_res (not (vector-ref *puzzle* k))))
                                              (if (> k size) (<change> 0 k) (<change> k 0))
                                              (__do_loop (+ k 1))))))
                        (__do_loop j)))))
         (puzzle-remove (lambda (i j)
                          (<change>
                             ()
                             *class*)
                          @sensitivity:FA
                          (let ((end (vector-ref *piecemax* i)))
                             (<change>
                                (letrec ((__do_loop (lambda (k)
                                                      @sensitivity:FA
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
                                   (+ (vector-ref *piececount* (vector-ref *class* i)) 1)))
                             (<change>
                                (vector-set!
                                   *piececount*
                                   (vector-ref *class* i)
                                   (+ (vector-ref *piececount* (vector-ref *class* i)) 1))
                                (letrec ((__do_loop (lambda (k)
                                                      @sensitivity:FA
                                                      (if (not (> k end))
                                                         #f
                                                         (begin
                                                            ((lambda (x) x) (if (vector-ref (vector-ref *p* i) k) (begin #f) #f))
                                                            (__do_loop (+ k 1)))))))
                                   (__do_loop 0))))))
         (trial (lambda (j)
                  (<change>
                     @sensitivity:FA
                     (let ((k 0)
                           (return #f))
                        (lambda (return)
                           (letrec ((__do_loop (lambda (i)
                                                 @sensitivity:FA
                                                 (if (let ((__or_res return)) (if __or_res (> i typemax) __or_res))
                                                    (begin
                                                       return)
                                                    (begin
                                                       (__do_loop (+ i 1)))))))
                              ((lambda (x) x) (__do_loop 0))))))
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
                        @sensitivity:FA
                        (<change>
                           ()
                           __do_loop)
                        (let ((index 0))
                           (letrec ((__do_loop (lambda (i)
                                                 (<change>
                                                    @sensitivity:FA
                                                    ((lambda (x) x) @sensitivity:FA))
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
                                                                                                               (<change>
                                                                                                                  (vector-set! (vector-ref *p* *iii*) index #t)
                                                                                                                  (__do_loop (+ k 1)))
                                                                                                               (<change>
                                                                                                                  (__do_loop (+ k 1))
                                                                                                                  (vector-set! (vector-ref *p* *iii*) index #t)))))))
                                                                                      (__do_loop 0))
                                                                                   (__do_loop (+ j 1)))))))
                                                          (__do_loop 0))
                                                       (__do_loop (+ i 1)))))))
                              (__do_loop 0))
                           (vector-set! *class* *iii* iclass)
                           (vector-set! *piecemax* *iii* index)
                           (if (not (= *iii* typemax))
                              (set! *iii* (+ *iii* 1))
                              #f))))
         (start (lambda ()
                  (<change>
                     @sensitivity:FA
                     ())
                  (set! *kount* 0)
                  (<change>
                     (letrec ((__do_loop (lambda (m)
                                           @sensitivity:FA
                                           (if (> m size)
                                              #f
                                              (begin
                                                 (vector-set! *puzzle* m #t)
                                                 (__do_loop (+ m 1)))))))
                        (__do_loop 0))
                     (letrec ((__do_loop (lambda (i)
                                           ((lambda (x) x) @sensitivity:FA)
                                           (if (> i 5)
                                              #f
                                              (begin
                                                 (letrec ((__do_loop (lambda (j)
                                                                       @sensitivity:FA
                                                                       (if (> j 5)
                                                                          #f
                                                                          (begin
                                                                             (letrec ((__do_loop (lambda (k)
                                                                                                   @sensitivity:FA
                                                                                                   (if (> k 5)
                                                                                                      #f
                                                                                                      (begin
                                                                                                         (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f)
                                                                                                         (__do_loop (+ k 1)))))))
                                                                                (__do_loop 1))
                                                                             (__do_loop (+ j 1)))))))
                                                    (__do_loop 1))
                                                 (__do_loop (+ i 1)))))))
                        (__do_loop 1)))
                  (<change>
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
                                                                                                   @sensitivity:FA
                                                                                                   (if (> k 5)
                                                                                                      #f
                                                                                                      (begin
                                                                                                         (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f)
                                                                                                         (__do_loop (+ k 1)))))))
                                                                                (__do_loop 1))
                                                                             (__do_loop (+ j 1)))))))
                                                    (__do_loop 1))
                                                 (__do_loop (+ i 1)))))))
                        (__do_loop 1))
                     (letrec ((__do_loop (lambda (m)
                                           @sensitivity:FA
                                           ((lambda (x) x)
                                              (if (> m size)
                                                 #f
                                                 (begin
                                                    (vector-set! *puzzle* m #t)
                                                    ((lambda (x) x) (__do_loop (+ m 1)))))))))
                        ((lambda (x) x) (__do_loop 0))))
                  (letrec ((__do_loop (lambda (i)
                                        @sensitivity:FA
                                        (if (> i typemax)
                                           (<change>
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
                                                 (__do_loop (+ i 1))))
                                           (<change>
                                              (begin
                                                 (letrec ((__do_loop (lambda (m)
                                                                       @sensitivity:FA
                                                                       (if (> m size)
                                                                          #f
                                                                          (begin
                                                                             (vector-set! (vector-ref *p* i) m #f)
                                                                             (__do_loop (+ m 1)))))))
                                                    (__do_loop 0))
                                                 (__do_loop (+ i 1)))
                                              #f)))))
                     (__do_loop 0))
                  (set! *iii* 0)
                  (<change>
                     (definePiece 0 3 1 0)
                     ((lambda (x) x) (definePiece 0 3 1 0)))
                  (definePiece 0 1 0 3)
                  (definePiece 0 0 3 1)
                  (definePiece 0 1 3 0)
                  (definePiece 0 3 0 1)
                  (definePiece 0 0 1 3)
                  (<change>
                     (definePiece 1 2 0 0)
                     (definePiece 1 0 2 0))
                  (<change>
                     (definePiece 1 0 2 0)
                     (definePiece 1 2 0 0))
                  (definePiece 1 0 0 2)
                  (definePiece 2 1 1 0)
                  (definePiece 2 1 0 1)
                  (definePiece 2 0 1 1)
                  (definePiece 3 1 1 1)
                  (<change>
                     ()
                     *)
                  (vector-set! *piececount* 0 13)
                  (<change>
                     (vector-set! *piececount* 1 3)
                     ())
                  (vector-set! *piececount* 2 1)
                  (vector-set! *piececount* 3 1)
                  (let ((m (+ (* *d* (+ *d* 1)) 1))
                        (n 0))
                     (<change>
                        (if (fit 0 m)
                           (set! n (place 0 m))
                           (begin
                              (newline)
                              (display "Error.")))
                        ())
                     (if (trial n) *kount* #f)))))
   (<change>
      (for-each
         (lambda (i)
            @sensitivity:FA
            (vector-set! *p* i (make-vector (+ size 1))))
         (my-iota (+ typemax 1)))
      (= (start) 2005))
   (<change>
      (= (start) 2005)
      (for-each
         (lambda (i)
            @sensitivity:FA
            (vector-set! *p* i (make-vector (+ size 1))))
         (my-iota (+ typemax 1)))))