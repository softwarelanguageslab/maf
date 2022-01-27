package maf.language.scheme.primitives

import maf.core._
import maf.language.scheme._
import maf.language.CScheme._

class BaseSchemePrelude:

    def primDefs = Map(
      "<=" -> "(define (<= x y) @sensitivity:FA (assert (number? x)) (or (< x y) (= x y)))",
      ">" -> "(define (> x y) @sensitivity:FA (assert (number? x)) (not (<= x y)))",
      ">=" -> "(define (>= x y) @sensitivity:FA (assert (number? x)) (or (> x y) (= x y)))",
      "abs" -> "(define (abs x) @sensitivity:FA (assert (number? x)) (if (< x 0) (- 0 x) x))",
      "append" ->
        """(define (append . lsts)
        |  (define (app lsts)
        |    @sensitivity:No
        |    (cond ((null? lsts) '())
        |          ((null? (cdr lsts)) (car lsts)) ; Structure sharing.
        |          (else (let loop ((first (car lsts))
        |                           (rest (app (cdr lsts))))
        |                  @sensitivity:No
        |                  (if (null? first)
        |                      rest
        |                      (cons (car first)
        |                            (loop (cdr first)
        |                                  rest)))))))
        |  (app lsts))""".stripMargin,
      "assoc" ->
        """(define (assoc k l)
        |  @sensitivity:FA
        |  (assert (list? l))
        |  (if (null? l)
        |    #f
        |   (if (equal? (caar l) k)
        |     (car l)
        |     (assoc k (cdr l)))))""".stripMargin,
      "assq" ->
        """(define (assq k l)
        |  @sensitivity:FA
        |  (assert (list? l))
        |  (if (null? l)
        |    #f
        |   (if (eq? (caar l) k)
        |     (car l)
        |     (assq k (cdr l)))))""".stripMargin,
      "assv" ->
        """(define (assv k l)
        |  @sensitivity:FA
        |  (assert (list? l))
        |  (if (null? l)
        |    #f
        |   (if (eqv? (caar l) k)
        |     (car l)
        |     (assq k (cdr l)))))""".stripMargin,
      "call-with-current-continuation" -> "(define call-with-current-continuation call/cc)",
      "call-with-input-file" ->
        """(define (call-with-input-file filename proc)
        |  (assert (string? filename))
        |  (assert (procedure? proc))
        |  (let* ((input-port (open-input-file filename))
        |         (res (proc input-port)))
        |    (close-input-port input-port)
        |    res))""".stripMargin,
      "call-with-output-file" ->
        """(define (call-with-output-file filename proc)
        |  (assert (string? filename))
        |  (assert (procedure? proc))
        |  (let* ((output-port (open-output-file filename))
        |         (res (proc output-port)))
        |    (close-output-port output-port)
        |    res))""".stripMargin,
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
      "char>?" -> "(define (char>? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (not (char<=? c1 c2)))",
      "char<=?" -> "(define (char<=? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (or (char<? c1 c2) (char=? c1 c2)))",
      "char>=?" -> "(define (char>=? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (or (char>? c1 c2) (char=? c1 c2)))",
      "char-ci>?" -> "(define (char-ci>? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (not (char-ci<=? c1 c2)))",
      "char-ci<=?" -> "(define (char-ci<=? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (or (char-ci<? c1 c2) (char-ci=? c1 c2)))",
      "char-ci>=?" -> "(define (char-ci>=? c1 c2) @sensitivity:FA (assert (char? c1)) (assert (char? c2)) (or (char-ci>? c1 c2) (char-ci=? c1 c2)))",
      "char-alphabetic?" -> "(define (char-alphabetic? c) @sensitivity:FA (and (char-ci>=? c #\\a) (char-ci<=? c #\\z)))",
      "char-numeric?" -> "(define (char-numeric? c) @sensitivity:FA (and (char<=? #\\0 c) (char<=? c #\\9)))",
      "char-whitespace?" -> "(define (char-whitespace? c) @sensitivity:FA (or (= (char->integer c) 9) (= (char->integer c) 10) (= (char->integer c) 32)))",
      "equal?" ->
        """(define (equal? a b)
        |  @sensitivity:FA
        |  (or (eq? a b)
        |    (and (null? a) (null? b))
        |    (and (string? a) (string? b) (string=? a b))
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
      "for-each" ->
        """(define (for-each f l)
        |  @sensitivity:1A
        |  (assert (procedure? f))
        |  (assert (list? l))
        |  (if (null? l)
        |      #t
        |      (if (pair? l)
        |          (begin (f (car l)) (for-each f (cdr l))))))""".stripMargin,
      "gcd" -> "(define (gcd a b) @sensitivity:FA (if (= b 0) a (gcd b (modulo a b))))",
      "lcm" -> "(define (lcm m n) @sensitivity:FA (/ (abs (* m n)) (gcd m n)))",
      "length" ->
        """(define (length l)
        |  @sensitivity:FA
        |  (assert (list? l))
        |  (letrec ((rec (lambda (l)
        |    (if (null? l)
        |       0
        |       (+ 1 (rec (cdr l)))))))
        |  (rec l)))""".stripMargin,
      "list" -> "(define (list . args) args)",
      "list-ref" ->
        """(define (list-ref l index)
        |  @sensitivity:FA
        |  (assert (list? l))
        |  (assert (number? index))
        |  (assert (< index (length l)))
        |  (if (= index 0)
        |    (car l)
        |    (list-ref (cdr l) (- index 1))))""".stripMargin,
      "list->string" ->
        """(define (list->string l)
        |   (assert (list? l))
        |   (if (null? l)
        |     ""
        |     (string-append (char->string (car l)) (list->string (cdr l)))))""".stripMargin,
      "list->vector" ->
        """(define (list->vector l)
        |  @sensitivity:FA
        |  (assert (list? l))
        |  (let ((v (make-vector (length l))))
        |    (let fill ((lst l) (i 0))
        |      (if (null? lst)
        |          v
        |          (begin (vector-set! v i (car lst))
        |                 (fill (cdr lst) (+ i 1)))))))""".stripMargin,
      "list-tail" ->
        """(define (list-tail x k)
        |  @sensitivity:FA
        |  (assert (list? l))
        |  (assert (numer? ))
        |  (if (zero? k)
        |    x
        |    (list-tail (cdr x) (- k 1))))""".stripMargin, // Based on definition in R5RS specification.
      "list?" -> "(define (list? l) @sensitivity:FA (or (and (pair? l) (list? (cdr l))) (null? l)))",
      "map" ->
        """(define (map f l)
        |  @sensitivity:1A
        |  (assert (procedure? f))
        |  (assert (list? l))
        |  (if (null? l)
        |      '()
        |      (cons (f (car l)) (map f (cdr l)))))""".stripMargin,
      "max" ->
        """(define (max ag0 . ags)
        | (let loop ((cur ags)
        |            (acc ag0))
        |   (if (null? cur)
        |       acc
        |       (let [(elm (car cur))
        |             (rst (cdr cur))]
        |         (loop rst (if (> elm acc) elm acc))))))""".stripMargin,
      "member" ->
        """(define (member e l)
        |  @sensitivity:FA
        |  (assert (list? l))
        |  (if (null? l)
        |    #f
        |    (if (equal? (car l) e)
        |      l
        |      (member e (cdr l)))))""".stripMargin,
      "memq" ->
        """(define (memq e l)
                |  @sensitivity:FA
                |  (assert (list? l))
                |  (if (null? l)
                |    #f
                |    (if (eq? (car l) e)
                |      l
                |      (memq e (cdr l)))))""".stripMargin,
      "memv" -> "(define (memv e l) @sensitivity:FA (memq e l))",
      "min" ->
        """(define (min ag0 . ags)
        | (let loop ((cur ags)
        |            (acc ag0))
        |   (if (null? cur)
        |       acc
        |       (let [(elm (car cur))
        |             (rst (cdr cur))]
        |         (loop rst (if (< elm acc) elm acc))))))""".stripMargin,
      "negative?" -> "(define (negative? x) @sensitivity:FA (assert (number? x)) (< x 0))",
      "newline" -> "(define (newline) @sensitivity:FA #f)", // undefined
      "not" -> "(define (not x) @sensitivity:FA (if x #f #t))",
      "odd?" -> "(define (odd? x) @sensitivity:FA (assert (number? x)) (= 1 (modulo x 2)))",
      "positive?" -> "(define (positive? x) @sensitivity:FA (assert (number? x)) (> x 0))",
      "reverse" ->
        """(define (reverse l)
        |  @sensitivity:No
        |  (assert (list? l))
        |  (if (null? l)
        |      '()
        |      (append (reverse (cdr l))
        |              (list (car l)))))""".stripMargin,
      "string<=?" -> "(define (string<=? s1 s2) @sensitivity:FA (or (string<? s1 s2) (string=? s1 s2)))",
      "string>?" -> "(define (string>? s1 s2) @sensitivity:FA (not (string<=? s1 s2)))",
      "string>=?" -> "(define (string>=? s1 s2) @sensitivity:FA (or (string>? s1 s2) (string=? s1 s2)))",
      "string" -> "(define (string . chars) (list->string chars))",
      "string-fill!" ->
        """(define (string-fill! s c)
        |  (let loop ((i (- (string-length s) 1)))
        |    (if (< i 0)
        |        #t
        |        (begin (string-set! s i c)
        |               (loop (- i 1))))))""".stripMargin,
      "string->list" ->
        """(define (string->list string)
        |  @sensitivity:FA
        |  (assert (string? string))
        |  (let ((len (string-length string)))
                        |  (let convert ((n (- len 1))
                        |                (r '()))
                        |    @sensitivity:FA
                        |    (if (< n 0)
                        |        r
                        |        (convert (- n 1)
                        |                 (cons (string-ref string n) r))))))""".stripMargin,
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
      "truncate" -> "(define (truncate x) @sensitivity:FA (assert (number? x)) (if (< x 0) (ceiling x) (floor x)))",
      "vector->list" ->
        """(define (vector->list v)
        |  @sensitivity:FA
        |  (assert (vector? v))
        |  (let construct ((i (- (vector-length v) 1)) (lst '()))
        |    @sensitivity:FA
        |    (if (< i 0)
        |        lst
        |        (construct (- i 1)
        |                   (cons (vector-ref v i) lst)))))""".stripMargin,
      "zero?" -> "(define (zero? x) @sensitivity:FA (assert (number? x)) (= x 0))",
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
      "void" -> "(define (void) #f)",
      "__toplevel_cons" -> "(define __toplevel_cons cons)",
      "__toplevel_car" -> "(define __toplevel_car car)",
      "__toplevel_cdr" -> "(define __toplevel_cdr cdr)",
      "__toplevel_set-car!" -> "(define __toplevel_set-car! set-car!)",
      "__toplevel_set-cdr!" -> "(define __toplevel_set-cdr! set-cdr!)",
      "__toplevel_append" -> "(define __toplevel_append append)",
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

    def parseDef(dff: String, nam: String): List[SchemeExp] =
      SchemeParser.parse(dff, Position.newTag(nam))

    lazy val primDefsParsed: Map[String, SchemeExp] = primDefs.map { case (nam, str) =>
      val exp = SchemeBody(parseDef(str, nam))
      (nam, exp)
    }

    val primNames: Set[String] = primDefs.keySet

    /** Transively adds all required definitions to the prelude, in addition to those mentioned in `incl`. */
    def addPrelude(prg: List[SchemeExp], incl: Set[String] = Set.empty): List[SchemeExp] =
        var prelude: List[SchemeExp] = List()
        var work: Set[String] = SchemeBody.fv(prg) ++ incl
        var added: Set[String] = Set.empty
        val primDefs = this.primDefs
        while work.nonEmpty do
            val free = work.filter(primDefs.contains).filterNot(added)
            val defs = free.map(primDefsParsed)
            prelude = defs.toList ::: prelude
            work = defs.flatMap(_.fv)
            added ++= free
        prelude ::: prg

object SchemePrelude extends BaseSchemePrelude
