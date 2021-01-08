package maf.language.scheme.primitives

import maf.core._
import maf.language.scheme._

object SchemePrelude {

  val primDefs = Map(
    "@sensitivity:1CS" -> "(define @sensitivity:1CS #f)",
    "@sensitivity:FA" -> "(define @sensitivity:FA #f)",
    "@sensitivity:1A" -> "(define @sensitivity:1A #f)",
    "@sensitivity:2A" -> "(define @sensitivity:2A #f)",
    "@sensitivity:No" -> "(define @sensitivity:No #f)",
    "abs" -> "(define (abs x) @sensitivity:FA (assert (number? x)) (if (< x 0) (- 0 x) x))",
    "append" -> """(define (append l1 l2)
                  |  @sensitivity:No
                  |  (assert (list? l1))
                  |  (assert (list? l2))
                  |  (if (null? l1)
                  |      l2
                  |      (cons (car l1)
                  |            (append (cdr l1) l2))))""".stripMargin,
    "assoc" -> """(define (assoc k l)
                 |  @sensitivity:FA
                 |  (assert (list? l))
                 |  (if (null? l)
                 |    #f
                 |   (if (equal? (caar l) k)
                 |     (car l)
                 |     (assoc k (cdr l)))))""".stripMargin,
    "assq" -> """(define (assq k l)
                |  @sensitivity:FA
                |  (assert (list? l))
                |  (if (null? l)
                |    #f
                |   (if (eq? (caar l) k)
                |     (car l)
                |     (assq k (cdr l)))))""".stripMargin,
    "assv" -> """(define (assv k l)
                |  @sensitivity:FA
                |  (assert (list? l))
                |  (if (null? l)
                |    #f
                |   (if (eqv? (caar l) k)
                |     (car l)
                |     (assq k (cdr l)))))""".stripMargin,
    "call-with-current-continuation" -> "(define call-with-current-continuation call/cc)",
    "display" -> "(define (display x) @sensitivity:FA x)", // undefined behavior in R5RS
    "equal?" -> """(define (equal? a b)
                  |  @sensitivity:FA
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
    "eqv?" -> "(define (eqv? x y) @sensitivity:FA (eq? x y))",
    "even?" -> "(define (even? x) @sensitivity:FA (= 0 (modulo x 2)))",
    // TODO: expt // TODO isn't this a primop (easier to handle real exponents).
    // TODO: exp
    "gcd" -> "(define (gcd a b) @sensitivity:FA (if (= b 0) a (gcd b (modulo a b))))",
    "lcm" -> "(define (lcm m n) @sensitivity:FA (/ (abs (* m n)) (gcd m n)))",
    "length" -> """(define (length l)
                  |  @sensitivity:FA
                  |  (assert (list? l))
                  |  (letrec ((rec (lambda (l)
                  |    (if (null? l)
                  |       0
                  |       (+ 1 (rec (cdr l)))))))
                  |  (rec l)))""".stripMargin,
    "list-ref" -> """(define (list-ref l index)
                    |  @sensitivity:FA
                    |  (assert (list? l))
                    |  (assert (number? index))
                    |  (assert (< index (length l)))
                    |  (if (= index 0)
                    |    (car l)
                    |    (list-ref (cdr l) (- index 1))))""".stripMargin,
    "list->vector" -> """(define (list->vector l)
                        |  @sensitivity:FA
                        |  (assert (list? l))
                        |  (let ((v (make-vector (length l))))
                        |    (let fill ((lst l) (i 0))
                        |      (if (null? lst)
                        |          v
                        |          (begin (vector-set! v i (car lst))
                        |                 (fill (cdr lst) (+ i 1)))))))""".stripMargin,
    "list-tail" -> """(define (list-tail x k)
                     |  @sensitivity:FA
                     |  (assert (list? l))
                     |  (assert (numer? ))
                     |  (if (zero? k)
                     |    x
                     |    (list-tail (cdr x) (- k 1))))""".stripMargin, // Based on definition in R5RS specification.
    "list?" -> "(define (list? l) @sensitivity:FA (or (and (pair? l) (list? (cdr l))) (null? l)))",
    //"max" -> "(define (max a b) (if (< a b) b a))", // Variadic => implemented manually.
    "member" -> """(define (member e l)
                  |  @sensitivity:FA
                  |  (assert (list? l))
                  |  (if (null? l)
                  |    #f
                  |    (if (equal? (car l) e)
                  |      l
                  |      (member e (cdr l)))))""".stripMargin,
    "memq" -> """(define (memq e l)
                |  @sensitivity:FA
                |  (assert (list? l))
                |  (if (null? l)
                |    #f
                |    (if (eq? (car l) e)
                |      l
                |      (memq e (cdr l)))))""".stripMargin,
    "memv" -> "(define (memv e l) @sensitivity:FA (memq e l))",
    //"min" -> "(define (min a b) (if (< a b) a b))", // Variadic => implemented manually.
    "negative?" -> "(define (negative? x) @sensitivity:FA (assert (number? x)) (< x 0))",
    "newline" -> "(define (newline) @sensitivity:FA #f)", // undefined
    "not" -> "(define (not x) @sensitivity:FA (if x #f #t))",
    "odd?" -> "(define (odd? x) @sensitivity:FA (assert (number? x)) (= 1 (modulo x 2)))",
    "positive?" -> "(define (positive? x) @sensitivity:FA (assert (number? x)) (> x 0))",
    "zero?" -> "(define (zero? x) @sensitivity:FA (assert (number? x)) (= x 0))",
    "<=" -> "(define (<= x y) @sensitivity:FA (assert (number? x)) (or (< x y) (= x y)))",
    ">" -> "(define (> x y) @sensitivity:FA (assert (number? x)) (not (<= x y)))",
    ">=" -> "(define (>= x y) @sensitivity:FA (assert (number? x)) (or (> x y) (= x y)))",
    "char>?" -> "(define (char>? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (not (char<=? c1 c2)))",
    "char<=?" -> "(define (char<=? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (or (char<? c1 c2) (char=? c1 c2)))",
    "char>=?" -> "(define (char>=? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (or (char>? c1 c2) (char=? c1 c2)))",
    "char-ci>?" -> "(define (char-ci>? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (not (char-ci<=? c1 c2)))",
    "char-ci<=?" -> "(define (char-ci<=? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (or (char-ci<? c1 c2) (char-ci=? c1 c2)))",
    "char-ci>=?" -> "(define (char-ci>=? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (or (char-ci>? c1 c2) (char-ci=? c1 c2)))",
    "char-alphabetic?" -> "(define (char-alphabetic? c) @sensitivity:FA (and (char-ci>=? c #\\a) (char-ci<=? c #\\z)))",
    "char-numeric?" -> "(define (char-alphabetic c) @sensitivity:FA (and (char<=? #\\0 c) (char<=? c #\\9)))",
    "caar" -> "(define (caar x) @sensitivity:FA (car (car x)))",
    "cadr" -> "(define (cadr x) @sensitivity:FA (car (cdr x)))",
    "cdar" -> "(define (cdar x) @sensitivity:FA (cdr (car x)))",
    "cddr" -> "(define (cddr x) @sensitivity:FA (cdr (cdr x)))",
    "caaar" -> "(define (caaar x) @sensitivity:FA (car (car (car x))))",
    "caadr" -> "(define (caadr x) @sensitivity:FA (car (car (cdr x))))",
    "cadar" -> "(define (cadar x) @sensitivity:FA (car (cdr (car x))))",
    "caddr" -> "(define (caddr x) @sensitivity:FA (car (cdr (cdr x))))",
    "cdaar" -> "(define (cdaar x) @sensitivity:FA (cdr (car (car x))))",
    "cdadr" -> "(define (cdadr x) @sensitivity:FA (cdr (car (cdr x))))",
    "cddar" -> "(define (cddar x) @sensitivity:FA (cdr (cdr (car x))))",
    "cdddr" -> "(define (cdddr x) @sensitivity:FA (cdr (cdr (cdr x))))",
    "caaaar" -> "(define (caaaar x) @sensitivity:FA (car (car (car (car x)))))",
    "caaadr" -> "(define (caaadr x) @sensitivity:FA (car (car (car (cdr x)))))",
    "caadar" -> "(define (caadar x) @sensitivity:FA (car (car (cdr (car x)))))",
    "caaddr" -> "(define (caaddr x) @sensitivity:FA (car (car (cdr (cdr x)))))",
    "cadaar" -> "(define (cadaar x) @sensitivity:FA (car (cdr (car (car x)))))",
    "cadadr" -> "(define (cadadr x) @sensitivity:FA (car (cdr (car (cdr x)))))",
    "caddar" -> "(define (caddar x) @sensitivity:FA (car (cdr (cdr (car x)))))",
    "cadddr" -> "(define (cadddr x) @sensitivity:FA (car (cdr (cdr (cdr x)))))",
    "cdaaar" -> "(define (cdaaar x) @sensitivity:FA (cdr (car (car (car x)))))",
    "cdaadr" -> "(define (cdaadr x) @sensitivity:FA (cdr (car (car (cdr x)))))",
    "cdadar" -> "(define (cdadar x) @sensitivity:FA (cdr (car (cdr (car x)))))",
    "cdaddr" -> "(define (cdaddr x) @sensitivity:FA (cdr (car (cdr (cdr x)))))",
    "cddaar" -> "(define (cddaar x) @sensitivity:FA (cdr (cdr (car (car x)))))",
    "cddadr" -> "(define (cddadr x) @sensitivity:FA (cdr (cdr (car (cdr x)))))",
    "cdddar" -> "(define (cdddar x) @sensitivity:FA (cdr (cdr (cdr (car x)))))",
    "cddddr" -> "(define (cddddr x) @sensitivity:FA (cdr (cdr (cdr (cdr x)))))",
    "vector->list" -> """(define (vector->list v)
                        |  @sensitivity:FA
                        |  (assert (vector? v))
                        |  (let construct ((i (- (vector-length v) 1)) (lst '()))
                        |    @sensitivity:FA
                        |    (if (< i 0)
                        |        lst
                        |        (construct (- i 1)
                        |                   (cons (vector-ref v i) lst)))))""".stripMargin,
    "reverse" -> """(define (reverse l)
                   |  @sensitivity:No
                   |  (assert (list? l))
                   |  (if (null? l)
                   |      '()
                   |      (append (reverse (cdr l))
                   |              (list (car l)))))""".stripMargin,
    "map" -> """(define (map f l)
                |  @sensitivity:1A
                |  (assert (procedure? f))
                |  (assert (list? l))
                |  (if (null? l)
                |      '()
                |      (cons (f (car l)) (map f (cdr l)))))""".stripMargin,
    "for-each" -> """(define (for-each f l)
                    |  @sensitivity:1A
                    |  (assert (procedure? f))
                    |  (assert (list? l))
                    |  (if (null? l)
                    |      #t
                    |      (if (pair? l)
                    |          (begin (f (car l)) (for-each f (cdr l))))))""".stripMargin,
    "string->list" -> """(define (string->list string)
                        |  @sensitivity:FA
                        |  (assert (string? string))
                        |  (define len (string-length string))
                        |  (let convert ((n (- len 1))
                        |                (r '()))
                        |    @sensitivity:FA
                        |    (if (< n 0)
                        |        r
                        |        (convert (- n 1)
                        |                 (cons (string-ref string n) r)))))""".stripMargin,
    "string=?" -> """(define (string=? s1 s2)
                    |  @sensitivity:FA
                    |  (assert (string? s1))
                    |  (assert (string? s2)) 
                    |  (and (= (string-length s1)(string-length s2))
                    |       (let loop ((i (- (string-length s1) 1)))
                    |        @sensitivity:FA
                    |        (if (< i 0)
                    |            #t
                    |            (and (char=? (string-ref s1 i) (string-ref s2 i))
                    |                 (loop (- i 1)))))))""".stripMargin,
    "string-ci=?" -> """(define (string-ci=? s1 s2)
                       |  @sensitivity:FA
                       |  (assert (string? s1))
                       |  (assert (string? s2))
                       |  (and (= (string-length s1)(string-length s2))
                       |       (let loop ((i (- (string-length s1) 1)))
                       |        @sensitivity:FA
                       |        (if (< i 0)
                       |            #t
                       |            (and (char-ci=? (string-ref s1 i) (string-ref s2 i))
                       |                 (loop (- i 1)))))))""".stripMargin,
    "string<=?" -> "(define (string<=? s1 s2) @sensitivity:FA (or (string<? s1 s2) (string=? s1 s2)))",
    "string>?" -> "(define (string>? s1 s2) @sensitivity:FA (not (string<=? s1 s2)))",
    "string>=?" -> "(define (string>=? s1 s2) @sensitivity:FA (or (string>? s1 s2) (string=? s1 s2)))",
    "list->string" ->
      """(define (list->string l)
        |   (assert (list? l))
        |   (if (null? l)
        |     ""
        |     (string-append (char->string (car l)) (list->string (cdr l)))))""".stripMargin,
    "truncate" -> "(define (truncate x) @sensitivity:FA (assert (number? x)) (if (< x 0) (ceiling x) (floor x)))",
    //"string-fill!" -> """(define (string-fill! s c)
    //                    |  (let loop ((i (- (string-length s) 1)))
    //                    |    (if (< i 0)
    //                    |        #t
    //                    |        (begin (string-set! s i c)
    //                    |               (loop (- i 1))))))""".stripMargin,

    //    "foldr" -> """(define (foldr f base lst) (foldr-aux f base lst))""",
    //    "foldr-aux" -> """(define (foldr-aux f base lst)
    //        (if (null? lst)
    //            base
    //            (f (car lst) (foldr-aux f base (cdr lst)))))""",
    //    "foldl" -> """(define (foldl f base lst) (foldl-aux f base lst))""",
    //    "foldl-aux" -> """(define (foldl-aux f base lst)
    //        (if (null? lst)
    //            base
    //            (foldl-aux f (f base (car lst)) (cdr lst))))"""
    // TODO: implement apply internally
    "apply" -> """(define (apply proc args)
                 |  @sensitivity:1A
                 |  (assert (procedure? proc))
                 |  (assert (list? args))
                 |  (cond
                 |    ((null?                args)   (proc))
                 |    ((null?        (   cdr args))  (proc (car args)))
                 |    ((null?        (  cddr args))  (proc (car args) (cadr args)))
                 |    ((null?        ( cdddr args))  (proc (car args) (cadr args) (caddr args)))
                 |    ((null?        (cddddr args))  (proc (car args) (cadr args) (caddr args) (cadddr args)))
                 |    ((null? (  cdr (cddddr args))) (proc (car args) (cadr args) (caddr args) (cadddr args) (car (cddddr args))))
                 |    ((null? ( cddr (cddddr args))) (proc (car args) (cadr args) (caddr args) (cadddr args) (car (cddddr args)) (cadr (cddddr args))))
                 |    ((null? (cdddr (cddddr args))) (proc (car args) (cadr args) (caddr args) (cadddr args) (car (cddddr args)) (cadr (cddddr args)) (caddr (cddddr args))))
                 |    (else (error "Unsupported call."))))""".stripMargin,
    "ref" -> "(define (ref x) @sensitivity:FA (cons x '()))",
    "deref" -> "(define deref car)",
    "ref-set" -> "(define ref-set set-car!)",
    "void" -> "(define (void) #f)"
    /*
    "ref" -> "(define (ref x) (cons x (new-lock))",
    "deref" ->
      """(define (deref r)
        |  (acquire (cdr r))
        |  (let ((value (car r)))
        |    (release (cdr r))
        |    value))""".stripMargin,
    "ref-set" ->
      """(define (ref-set r value)
        |  (acquire (cdr r))
        |  (set-car! r value)
        |  (release (cdr r))
        |  value)""".stripMargin
     */
  )

  val primDefsParsed: Map[String, SchemeExp] = primDefs.map { case (nam, str) =>
    val exp = SchemeParser.parse(str, Position.newTag(nam))
    (nam, exp)
  }

  val primNames: Set[String] = primDefs.keySet

  /** Transively adds all required definitions to the prelude, except the ones listed in `excl`. */
  def addPrelude(exp: SchemeExp, excl: Set[String] = Set()): SchemeExp = {
    var prelude: List[SchemeExp] = List()
    var work: List[Expression] = List(exp)
    var visited: Set[String] = Set()

    while (work.nonEmpty)
      work.head match {
        case Identifier(name, _) if primNames.contains(name) && !visited.contains(name) && !excl.contains(name) =>
          val exp = primDefsParsed(name)
          prelude = exp :: prelude
          work = exp :: work.tail // If a primitive depends on other primitives, make sure to also inline them.
          visited = visited + name
        case e => work = e.subexpressions ::: work.tail // There will be no subexpressions if e is an Identifier for which the conditions do not hold.
      }
    SchemeBody(prelude ::: List(exp))
  }
}
