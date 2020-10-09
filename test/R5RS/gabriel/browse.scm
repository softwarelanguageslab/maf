;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.

(define (lookup key table)
  @sensitivity:FA
  (let loop ((x table))
    @sensitivity:FA
    (if (null? x)
        #f
        (let ((pair (car x)))
          (if (eq? (car pair) key)
              pair
              (loop (cdr x)))))))

(define properties '())

(define (get key1 key2)
  @sensitivity:FA
  (let ((x (lookup key1 properties)))
    (if x
        (let ((y (lookup key2 (cdr x))))
          (if y
              (cdr y)
              #f))
        #f)))

(define (put key1 key2 val)
  @sensitivity:FA
  (let ((x (lookup key1 properties)))
    (if x
        (let ((y (lookup key2 (cdr x))))
          (if y
              (set-cdr! y val)
              (set-cdr! x (cons (cons key2 val) (cdr x)))))
        (set! properties
              (cons (list key1 (cons key2 val)) properties)))))

(define *current-gensym* 0)

(define (generate-symbol)
  @sensitivity:FA
  (set! *current-gensym* (+ *current-gensym* 1))
  (string->symbol (number->string *current-gensym*)))

(define (append-to-tail! x y)
  @sensitivity:FA
  (if (null? x)
      y
      (do ((a x b)
           (b (cdr x) (cdr b)))
        ((null? b)
         (set-cdr! a y)
         x)
        @sensitivity:FA)))

(define (tree-copy x)
  @sensitivity:FA
  (if (not (pair? x))
      x
      (cons (tree-copy (car x))
            (tree-copy (cdr x)))))

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(define *rand* 21)

(define (init n m npats ipats)
  @sensitivity:FA
  (let ((ipats (tree-copy ipats)))
    (do ((p ipats (cdr p)))
        ((null? (cdr p)) (set-cdr! p ipats))
      @sensitivity:FA)
    (do ((n n (- n 1))
         (i m (cond ((zero? i) m)
                    (else (- i 1))))
         (name (generate-symbol) (generate-symbol))
         (a '()))
        ((= n 0) a)
      @sensitivity:No ;; does not terminate with FA, 1A
      (set! a (cons name a))
      (do ((i i (- i 1)))
          ((zero? i))
        @sensitivity:FA
        (put name (generate-symbol) #f))
      (put name
           'pattern
           (do ((i npats (- i 1))
                (ipats ipats (cdr ipats))
                (a '()))
               ((zero? i) a)
             @sensitivity:FA
             (set! a (cons (car ipats) a))))
      (do ((j (- m i) (- j 1)))
          ((zero? j))
        @sensitivity:FA
        (put name (generate-symbol) #f)))))

(define (browse-random)
  @sensitivity:FA
  (set! *rand* (remainder (* *rand* 17) 251))
  *rand*)

(define (randomize l)
  @sensitivity:FA
  (do ((a '()))
      ((null? l) a)
    @sensitivity:No ;; does not terminate with FA
    (let ((n (remainder (browse-random) (length l))))
      (cond ((zero? n)
             (set! a (cons (car l) a))
             (set! l (cdr l))
             l)
            (else
             (do ((n n (- n 1))
                  (x l (cdr x)))
               ((= n 1)
                (set! a (cons (cadr x) a))
                (set-cdr! x (cddr x))
                x)
               @sensitivity:FA))))))

(define (my-match pat dat alist)
  @sensitivity:No ;; does not terminate with FA
  (cond ((null? pat)
         (null? dat))
        ((null? dat) '())
        ((or (eq? (car pat) '?)
             (eq? (car pat)
                  (car dat)))
         (my-match (cdr pat) (cdr dat) alist))
        ((eq? (car pat) '*)
         (or (my-match (cdr pat) dat alist)
             (my-match (cdr pat) (cdr dat) alist)
             (my-match pat (cdr dat) alist)))
        (else (cond ((not (pair? (car pat)))
                     (cond ((eq? (string-ref (symbol->string (car pat)) 0)
                                 #\?)
                            (let ((val (assq (car pat) alist)))
                              (cond (val (my-match (cons (cdr val)
                                                         (cdr pat))
                                                   dat alist))
                                    (else (my-match (cdr pat)
                                                    (cdr dat)
                                                    (cons (cons (car pat)
                                                                (car dat))
                                                          alist))))))
                           ((eq? (string-ref (symbol->string (car pat)) 0)
                                 #\*)
                            (let ((val (assq (car pat) alist)))
                              (cond (val (my-match (append (cdr val)
                                                           (cdr pat))
                                                   dat alist))
                                    (else
                                     (do ((l '()
                                             (append-to-tail!
                                              l
                                              (cons (if (null? d)
                                                        '()
                                                        (car d))
                                                    '())))
                                          (e (cons '() dat) (cdr e))
                                          (d dat (if (null? d) '() (cdr d))))
                                       ((or (null? e)
                                            (my-match (cdr pat)
                                                      d
                                                      (cons
                                                       (cons (car pat) l)
                                                       alist)))
                                        (if (null? e) #f #t))
                                       @sensitivity:1A ;; does not terminate with FA
                                       )))))
                           (else #f))) ;;;; fix suggested by Manuel Serrano (cond did not have an else clause); this changes the run time quite a bit
                    (else (and
                           (pair? (car dat))
                           (my-match (car pat)
                                     (car dat) alist)
                           (my-match (cdr pat)
                                     (cdr dat) alist)))))))

(define database
  (randomize
   (init 100 10 4 '((a a a b b b b a a a a a b b a a a)
                    (a a b b b b a a
                       (a a)(b b))
                    (a a a b (b a) b a b a)))))

(define (browse pats)
  @sensitivity:FA
  (investigate
   database
   pats))

(define (investigate units pats)
  @sensitivity:FA
  (do ((units units (cdr units)))
      ((null? units))
    @sensitivity:FA
    (do ((pats pats (cdr pats)))
        ((null? pats))
      @sensitivity:FA
      (do ((p (get (car units) 'pattern)
              (cdr p)))
          ((null? p))
        @sensitivity:FA
        (my-match (car pats) (car p) '())))))

(browse '((*a ?b *b ?b a *a a *b *a)(*a *b *b *a (*a) (*b))(? ? * (b a) * ? ?)))
*current-gensym*
