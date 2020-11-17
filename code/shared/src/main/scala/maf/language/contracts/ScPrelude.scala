package maf.language.contracts

object ScPrelude {
  val preludeFunctions = Map(
    "abs"     -> "(define (abs x) @sensitivity:FA (assert (number? x)) (if (< x 0) (- 0 x) x))",
    "append"  -> """(define (append l1 l2)
                |  @sensitivity:No
                |  (assert (list? l1))
                |  (assert (list? l2))
                |  (if (null? l1)
                |      l2
                |      (cons (car l1)
                |            (append (cdr l1) l2))))""".stripMargin,
    "assoc"   -> """(define/contract (assoc k l) (~> list? any?)
               |  (if (null? l)
               |    #f
               |   (if (equal? (caar l) k)
               |     (car l)
               |     (assoc k (cdr l)))))""".stripMargin,
    "assq"    -> """(define/contract (assq k l) (~> list? any?)
              |  (if (null? l)
              |    #f
              |   (if (eq? (caar l) k)
              |     (car l)
              |     (assq k (cdr l)))))""".stripMargin,
    "assv"    -> """(define/contract (assv k l) (~> list? any?)
              |  (if (null? l)
              |    #f
              |   (if (eqv? (caar l) k)
              |     (car l)
              |     (assq k (cdr l)))))""".stripMargin,
    "display" -> "(define (display x) x)", // undefined behavior in R5RS
    "equal?"  -> """(define (equal? a b)
                |  (or (eq? a b)
                |    (and (null? a) (null? b))
                |    (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
                |    (and (vector? a) (vector? b)
                |      (let ((n (vector-length a)))
                |        (and (= (vector-length b) n)
                |          (letrec ((loop (lambda (i)
                |                           @sensitivity:FA
                |                           (or (= i n)
                |                             (and (equal? (vector-ref a i) (vector-ref b i))
                |                               (loop (+ i 1)))))))
                |            (loop 0)))))))""".stripMargin,
    "eqv?"    -> "(define/contract (eqv? x y) (~> (any? any?) bool?) (eq? x y))",
    "even?"   -> "(define/contract (even? x) (~> number? bool?) (= 0 (modulo x 2)))",
    // TODO: expt // TODO isn't this a primop (easier to handle real exponents).
    // TODO: exp
    "gcd"          -> "(define/contract (gcd a b) (~> (number? number?) number?) (if (= b 0) a (gcd b (modulo a b))))",
    "lcm"          -> "(define/contract (lcm m n) (~> (number? number?) number?) (/ (abs (* m n)) (gcd m n)))",
    "length"       -> """(define/contract (length l) (~> list? number?)
                |  (letrec ((rec (lambda (l)
                |    (if (null? l)
                |       0
                |       (+ 1 (rec (cdr l)))))))
                |  (rec l)))""".stripMargin,
    "list-ref"     -> """(define/contract (list-ref l index) (~> (list? number?) any?)
                  |  (if (= index 0)
                  |    (car l)
                  |    (list-ref (cdr l) (- index 1))))""".stripMargin,
    "list->vector" -> """(define/contract (list->vector l) 
                      |  (~> list? vector?)
                      |  (let ((v (make-vector (length l))))
                      |    (let fill ((lst l) (i 0))
                      |      (if (null? lst)
                      |          v
                      |          (begin (vector-set! v i (car lst))
                      |                 (fill (cdr lst) (+ i 1)))))))""".stripMargin,
    "list-tail"    -> """(define/contract (list-tail x k)
                   |  (~> (list? number?) list?)
                   |  (if (zero? k)
                   |    x
                   |    (list-tail (cdr x) (- k 1))))""".stripMargin, // Based on definition in R5RS specification.
    "list?"        -> "(define (list? l) @sensitivity:FA (or (and (pair? l) (list? (cdr l))) (null? l)))",
    //"max" -> "(define (max a b) (if (< a b) b a))", // Variadic => implemented manually.
    "member" -> """(define/contract (member e l)
                |  (~> (any? list?) any?)
                |  (if (null? l)
                |    #f
                |    (if (equal? (car l) e)
                |      l
                |      (member e (cdr l)))))""".stripMargin,
    "memq"   -> """(define/contract (memq e l)
              |  (~> (any? list?) any?)
              |  (if (null? l)
              |    #f
              |    (if (eq? (car l) e)
              |      l
              |      (memq e (cdr l)))))""".stripMargin,
    "memv"   -> "(define/contract (memv e l) (~> (any? list?) any?) (memq e l))",
    //"min" -> "(define (min a b) (if (< a b) a b))", // Variadic => implemented manually.
    "negative?"    -> "(define/contract (negative? x) (~> number? bool?) (< x 0))",
    "newline"      -> "(define (newline) @sensitivity:FA #f)", // undefined
    "not"          -> "(define (not x) @sensitivity:FA (if x #f #t))",
    "odd?"         -> "(define (odd? x) @sensitivity:FA (assert (number? x)) (= 1 (modulo x 2)))",
    "positive?"    -> "(define (positive? x) @sensitivity:FA (assert (number? x)) (> x 0))",
    "zero?"        -> "(define (zero? x) @sensitivity:FA (assert (number? x)) (= x 0))",
    "<="           -> "(define (<= x y) @sensitivity:FA (assert (number? x)) (or (< x y) (= x y)))",
    ">"            -> "(define (> x y) @sensitivity:FA (assert (number? x)) (not (<= x y)))",
    ">="           -> "(define (>= x y) @sensitivity:FA (assert (number? x)) (or (> x y) (= x y)))",
    "char>?"       -> "(define (char>? c1 c2) @sensitivity:FA (assert (char? x)) (not (char<=? c1 c2)))",
    "char<=?"      -> "(define (char<=? c1 c2) @sensitivity:FA (assert (char? x)) (or (char<? c1 c2) (char=? c1 c2)))",
    "char>=?"      -> "(define (char<=? c1 c2) @sensitivity:FA (assert (char? x)) (or (char>? c1 c2) (char=? c1 c2)))",
    "char-ci>?"    -> "(define (char-ci>? c1 c2) @sensitivity:FA (assert (char? x)) (not (char-ci<=? c1 c2)))",
    "char-ci<=?"   -> "(define (char-ci<=? c1 c2) @sensitivity:FA (or (char-ci<? c1 c2) (char-ci=? c1 c2)))",
    "char-ci>=?"   -> "(define (char-ci<=? c1 c2) @sensitivity:FA (or (char-ci>? c1 c2) (char-ci=? c1 c2)))",
    "caar"         -> "(define (caar x) @sensitivity:FA (car (car x)))",
    "cadr"         -> "(define (cadr x) @sensitivity:FA (car (cdr x)))",
    "cdar"         -> "(define (cdar x) @sensitivity:FA (cdr (car x)))",
    "cddr"         -> "(define (cddr x) @sensitivity:FA (cdr (cdr x)))",
    "caaar"        -> "(define/contract (caaar x) (~> pair? any?)  (car (car (car x))))",
    "caadr"        -> "(define/contract (caadr x) (~> pair? any?)  (car (car (cdr x))))",
    "cadar"        -> "(define/contract (cadar x) (~> pair? any?)  (car (cdr (car x))))",
    "caddr"        -> "(define/contract (caddr x) (~> pair? any?)  (car (cdr (cdr x))))",
    "cdaar"        -> "(define/contract (cdaar x) (~> pair? any?)  (cdr (car (car x))))",
    "cdadr"        -> "(define/contract (cdadr x) (~> pair? any?)  (cdr (car (cdr x))))",
    "cddar"        -> "(define/contract (cddar x) (~> pair? any?)  (cdr (cdr (car x))))",
    "cdddr"        -> "(define/contract (cdddr x) (~> pair? any?)  (cdr (cdr (cdr x))))",
    "caaaar"       -> "(define/contract (caaaar x) (~> pair? any?) (car (car (car (car x)))))",
    "caaadr"       -> "(define/contract (caaadr x) (~> pair? any?) (car (car (car (cdr x)))))",
    "caadar"       -> "(define/contract (caadar x) (~> pair? any?) (car (car (cdr (car x)))))",
    "caaddr"       -> "(define/contract (caaddr x) (~> pair? any?) (car (car (cdr (cdr x)))))",
    "cadaar"       -> "(define/contract (cadaar x) (~> pair? any?) (car (cdr (car (car x)))))",
    "cadadr"       -> "(define/contract (cadadr x) (~> pair? any?) (car (cdr (car (cdr x)))))",
    "caddar"       -> "(define/contract (caddar x) (~> pair? any?) (car (cdr (cdr (car x)))))",
    "cadddr"       -> "(define/contract (cadddr x) (~> pair? any?) (car (cdr (cdr (cdr x)))))",
    "cdaaar"       -> "(define/contract (cdaaar x) (~> pair? any?) (cdr (car (car (car x)))))",
    "cdaadr"       -> "(define/contract (cdaadr x) (~> pair? any?) (cdr (car (car (cdr x)))))",
    "cdadar"       -> "(define/contract (cdadar x) (~> pair? any?) (cdr (car (cdr (car x)))))",
    "cdaddr"       -> "(define/contract (cdaddr x) (~> pair? any?) (cdr (car (cdr (cdr x)))))",
    "cddaar"       -> "(define/contract (cddaar x) (~> pair? any?) (cdr (cdr (car (car x)))))",
    "cddadr"       -> "(define/contract (cddadr x) (~> pair? any?) (cdr (cdr (car (cdr x)))))",
    "cdddar"       -> "(define/contract (cdddar x) (~> pair? any?) (cdr (cdr (cdr (car x)))))",
    "cddddr"       -> "(define/contract (cddddr x) (~> pair? any?) (cdr (cdr (cdr (cdr x)))))",
    "vector->list" -> """(define/contract (vector->list v) (~> vector? list?)
                      |  (let construct ((i (- (vector-length v) 1)) (lst '()))
                      |    (if (< i 0)
                      |        lst
                      |        (construct (- i 1)
                      |                   (cons (vector-ref v i) lst)))))""".stripMargin,
    "reverse"      -> """(define/contract (reverse l) (~> list? list?)
                 |  (if (null? l)
                 |      '()
                 |      (append (reverse (cdr l))
                 |              (list (car l)))))""".stripMargin,
    "map"          -> """(define/contract (map f l) (~> (proc? list?) list?)
              |  (if (null? l)
              |      '()
              |      (cons (f (car l)) (map f (cdr l)))))""".stripMargin,
    "for-each"     -> """(define/contract (for-each f l) (~> (proc? list?) list?)
                  |  (if (null? l)
                  |      #t
                  |      (if (pair? l)
                  |          (begin (f (car l)) (for-each f (cdr l))))))""".stripMargin,
    "string->list" -> """(define/contract (string->list string) (~> string? list)
                      |  (define len (string-length string))
                      |  (let convert ((n (- len 1))
                      |                (r '()))
                      |    @sensitivity:FA
                      |    (if (< n 0)
                      |        r
                      |        (convert (- n 1)
                      |                 (cons (string-ref string n) r)))))""".stripMargin,
    "string=?"     -> """(define/contract (string=? s1 s2) 
                  |   (~> (string? string?) bool?)
                  |  (and (= (string-length s1)(string-length s2))
                  |       (let loop ((i (- (string-length s1) 1)))
                  |        @sensitivity:FA
                  |        (if (< i 0)
                  |            #t
                  |            (and (char=? (string-ref s1 i) (string-ref s2 i))
                  |                 (loop (- i 1)))))))""".stripMargin,
    "string-ci=?"  -> """(define/contract (string-ci=? s1 s2)
                     |   (~> (string? string?) bool?)
                     |  (and (= (string-length s1)(string-length s2))
                     |       (let loop ((i (- (string-length s1) 1)))
                     |        @sensitivity:FA
                     |        (if (< i 0)
                     |            #t
                     |            (and (char-ci=? (string-ref s1 i) (string-ref s2 i))
                     |                 (loop (- i 1)))))))""".stripMargin,
    "string<=?"    -> "(define (string<=? s1 s2) (or (string<? s1 s2) (string=? s1 s2)))",
    "string>?"     -> "(define (string>? s1 s2) (not (string<=? s1 s2)))",
    "string>=?"    -> "(define (string<=? s1 s2) (or (string>? s1 s2) (string=? s1 s2)))",
    "truncate"     -> "(define (truncate x) (assert (number? x)) (if (< x 0) (ceiling x) (floor x)))"
  )

  val prelude: List[ScExp] = preludeFunctions.values.map(SCExpCompiler.read).toList
}
