; Change definition of lookup to an equivalent primitive. (source: "test/R5RS/gambit/browse.scm")

;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.

(define lookup ; (lookup key table)
  (<change>
    (lambda (key table)
      (let loop ((x table))
        (if (null? x)
          #f
          (let ((pair (car x)))
            (if (eq? (car pair) key)
              pair
              (loop (cdr x)))))))
    assq))

(define properties '())

(define (get key1 key2)
  (let ((x (lookup key1 properties)))
    (if x
      (let ((y (lookup key2 (cdr x))))
        (if y
          (cdr y)
          #f))
      #f)))

(define (put key1 key2 val)
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
  (set! *current-gensym* (+ *current-gensym* 1))
  (string->symbol (number->string *current-gensym*)))










(define (tree-copy x)
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
  (let ((ipats (tree-copy ipats)))
    (do ((p ipats (cdr p)))
      ((null? (cdr p)) (set-cdr! p ipats)))
    (do ((n n (- n 1))
          (i m (cond ((zero? i) m)
                 (else (- i 1))))
          (name (generate-symbol) (generate-symbol))
          (a '()))
      ((= n 0) a)
      (set! a (cons name a))
      (do ((i i (- i 1)))
        ((zero? i))
        (put name (generate-symbol) #f))
      (put name
        'pattern
        (do ((i npats (- i 1))
              (ipats ipats (cdr ipats))
              (a '()))
          ((zero? i) a)
          (set! a (cons (car ipats) a))))
      (do ((j (- m i) (- j 1)))
        ((zero? j))
        (put name (generate-symbol) #f)))))

(define (my-match pat dat alist)
  (cond ((null? pat)
          (null? dat))
    ((null? dat) '())
    ((or (eq? (car pat) '?)
       (eq? (car pat)
         (car dat)))
      (my-match (cdr pat) (cdr dat) alist))
    ((not (pair? (car pat)))
      #f) ;;;; fix suggested by Manuel Serrano (cond did not have an else clause); this changes the run time quite a bit
    (else (and
            (pair? (car dat))
            (my-match (car pat)
              (car dat) alist)
            (my-match (cdr pat)
              (cdr dat) alist)))))

(define database
  (init 100 10 4 '((a a b b b b a a (a a)(b b))
                    (a a a b (b a) b a b a))))

(define (investigate units pats)
  (do ((units units (cdr units)))
    ((null? units))
    (do ((pats pats (cdr pats)))
      ((null? pats))
      (do ((p (get (car units) 'pattern)
             (cdr p)))
        ((null? p))
        (my-match (car pats) (car p) '())))))

(investigate database '((*a *b a *a a *b *a)(? ? (b a) ? ?)))
*current-gensym*