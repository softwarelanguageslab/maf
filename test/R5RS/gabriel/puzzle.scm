;;; PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in Pascal.

(define (my-iota n)
  @sensitivity:FA
  (do ((n n (- n 1))
       (list '() (cons (- n 1) list)))
      ((zero? n) list)
    @sensitivity:FA))

(define size 511)
(define classmax 3)
(define typemax 12)

(define *iii* 0)
(define *kount* 0)
(define *d* 8)

(define *piececount* (make-vector (+ classmax 1) 0))
(define *class* (make-vector (+ typemax 1) 0))
(define *piecemax* (make-vector (+ typemax 1) 0))
(define *puzzle* (make-vector (+ size 1)))
(define *p* (make-vector (+ typemax 1)))

(define (fit i j)
  @sensitivity:FA
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((or (> k end)
             (and (vector-ref (vector-ref *p* i) k)
                  (vector-ref *puzzle* (+ j k))))
         (if (> k end) #t #f))
      @sensitivity:No ;; does not terminate with FA
      )))

(define (place i j)
  @sensitivity:FA
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
      @sensitivity:FA
      (cond ((vector-ref (vector-ref *p* i) k)
             (vector-set! *puzzle* (+ j k) #t)
             #t)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (- (vector-ref *piececount* (vector-ref *class* i)) 1))
    (do ((k j (+ k 1)))
        ((or (> k size) (not (vector-ref *puzzle* k)))
         (if (> k size) 0 k))
      @sensitivity:FA)))

(define (puzzle-remove i j)
  @sensitivity:FA
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
      @sensitivity:FA
      (cond ((vector-ref (vector-ref *p* i) k)
             (vector-set! *puzzle* (+ j k) #f)
             #f)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (+ (vector-ref *piececount* (vector-ref *class* i)) 1))))

(define (trial j)
  @sensitivity:FA
  (let ((k 0) (return #f))
    (lambda (return)
      (do ((i 0 (+ i 1)))
          ((or return (> i typemax)) (set! *kount* (+ *kount* 1)) return)
        @sensitivity:FA
        (cond
         ((not
           (zero?
            (vector-ref *piececount* (vector-ref *class* i))))
          (cond
           ((fit i j)
            (set! k (place i j))
            (cond
             ((or (trial k) (zero? k))
              (set! *kount* (+ *kount* 1))
              (set! return #t))
             (else (puzzle-remove i j)))))))))))

(define (definePiece iclass ii jj kk)
  @sensitivity:FA
  (let ((index 0))
    (do ((i 0 (+ i 1)))
        ((> i ii))
      @sensitivity:FA
      (do ((j 0 (+ j 1)))
          ((> j jj))
        @sensitivity:FA
        (do ((k 0 (+ k 1)))
            ((> k kk))
          @sensitivity:FA
          (set! index (+ i (* *d* (+ j (* *d* k)))))
          (vector-set! (vector-ref *p* *iii*) index  #t))))
    (vector-set! *class* *iii* iclass)
    (vector-set! *piecemax* *iii* index)
    (cond ((not (= *iii* typemax))
           (set! *iii* (+ *iii* 1))))))

(define (start)
  @sensitivity:FA
  (set! *kount* 0)
  (do ((m 0 (+ m 1)))
      ((> m size))
    @sensitivity:FA
    (vector-set! *puzzle* m #t))
  (do ((i 1 (+ i 1)))
      ((> i 5))
    @sensitivity:FA
    (do ((j 1 (+ j 1)))
        ((> j 5))
      @sensitivity:FA
      (do ((k 1 (+ k 1)))
          ((> k 5))
        @sensitivity:FA
        (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f))))
  (do ((i 0 (+ i 1)))
      ((> i typemax))
    @sensitivity:FA
    (do ((m 0 (+ m 1)))
        ((> m size))
      @sensitivity:FA
      (vector-set! (vector-ref *p* i) m #f)))
  (set! *iii* 0)
  (definePiece 0 3 1 0)
  (definePiece 0 1 0 3)
  (definePiece 0 0 3 1)
  (definePiece 0 1 3 0)
  (definePiece 0 3 0 1)
  (definePiece 0 0 1 3)

  (definePiece 1 2 0 0)
  (definePiece 1 0 2 0)
  (definePiece 1 0 0 2)

  (definePiece 2 1 1 0)
  (definePiece 2 1 0 1)
  (definePiece 2 0 1 1)

  (definePiece 3 1 1 1)

  (vector-set! *piececount* 0 13)
  (vector-set! *piececount* 1 3)
  (vector-set! *piececount* 2 1)
  (vector-set! *piececount* 3 1)
  (let ((m (+ (* *d* (+ *d* 1)) 1))
        (n 0))
    (cond ((fit 0 m) (set! n (place 0 m)))
          (else (begin (newline) (display "Error."))))
    (if (trial n)
      *kount*
      #f)))

(for-each (lambda (i) @sensitivity:FA (vector-set! *p* i (make-vector (+ size 1))))
          (my-iota (+ typemax 1)))

(= (start) 2005)