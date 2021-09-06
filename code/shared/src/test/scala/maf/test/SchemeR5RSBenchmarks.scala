package maf.test

object SchemeR5RSBenchmarks:

    val bench: List[(String, Any)] = List(
      ("'()", Nil),
      ("'a", Symbol("a")),
      ("((if #f + *) 3 4)", 12),
      ("(and (= 2 2) (< 2 1))", false),
      ("(and (= 2 2) (> 2 1))", true),
      ("(and)", true),
      ("(or #f #f #f)", false),
      ("(or (= 2 2) (< 2 1))", true),
      ("(or (= 2 2) (> 2 1))", true),
      ("(quote a)", Symbol("a")),
      // §6.1 Equivalence predicates
      // (eqv? obj1 obj2)
      ("(eqv? #f 'nil)", false),
      ("(eqv? '() '())", true),
      ("(eqv? 'a 'a)", true),
      ("(eqv? 'a 'b)", false),
      ("(eqv? (cons 1 2) (cons 1 2))", false),
      ("(eqv? (lambda () 1) (lambda () 2))", false),
      ("(eqv? 100000000 100000000)", true),
      ("(eqv? 2 2)", true),
      // (eq? obj1 obj2)
      ("(eq? '() '())", true),
      ("(eq? 'a 'a)", true),
      ("(eq? (cons 'a '()) (cons 'a '()))", false),
      ("(eq? (list 'a) (list 'a))", false),
      ("(eq? car car)", true),
      // (equal? obj1 obj2)
      ("(equal? #\\a #\\b)", false),
      ("(equal? '(a (b) c) '(a (b) c))", true),
      ("(equal? '(a b c) '(a b))", false),
      ("(equal? '(a b c) '(a c b))", false),
      ("(equal? '(a) '(a))", true),
      ("(equal? 'a 'a)", true),
      ("(equal? 1 2)", false),
      ("(equal? 2 2)", true),
      ("(equal? \"abc\" \"abc\")", true),
      // §6.2.5 Numerical operations
      // (number? obj)
      ("(number? '())", false),
      ("(number? -1)", true),
      ("(number? 0)", true),
      ("(number? 0.5)", true),
      // (complex? obj) not supported
      // (real? obj)
      ("(real? 1.5)", true),
      ("(real? 3)", true),
      // (rational? obj) not supported
      // (integer? obj)
      ("(integer? '())", false),
      ("(integer? 0)", true),
      // (exact? obj) not supported
      // (inexact? obj) not supported
      // (= z1 z2 z3 ...)
      ("(= 1 1)", true),
      ("(= 2 1)", false),
      // (< x1 x2 x3 ...) No support for multiple arguments
      ("(< (- (tan 4) (/ (sin 4) (cos 4))) 0.0001)", true),
      ("(< 1 1)", false),
      ("(< 1 2)", true),
      ("(< 2 1)", false),
      ("(< 2.0 2.1)", true),
      ("(< 2.1 2.0)", false),
      // (> x1 x2 x3 ...)
      ("(> 1 1)", false),
      ("(> 1 2)", false),
      ("(> 2 1)", true),
      ("(> 2.0 2.1)", false),
      ("(> 2.1 2.0)", true),
      // (<= x1 x2 x3 ...)
      ("(<= 1 1)", true),
      ("(<= 1 2)", true),
      ("(<= 2 1)", false),
      ("(<= 2.0 2.1)", true),
      ("(<= 2.1 2.0)", false),
      // (>= x1 x2 x3 ...)
      ("(>= 1 1)", true),
      ("(>= 1 2)", false),
      ("(>= 2 1)", true),
      ("(>= 2.0 2.1)", false),
      ("(>= 2.1 2.0)", true),
      // (zero? z)
      ("(zero? -1)", false),
      ("(zero? 0)", true),
      ("(zero? 1)", false),
      // (positive? x)
      ("(positive? -1)", false),
      ("(positive? 0)", false),
      ("(positive? 1)", true),
      // (negative? x)
      ("(negative? -1)", true),
      ("(negative? 0)", false),
      ("(negative? 1)", false),
      // (odd? n)
      ("(odd? 0)", false),
      ("(odd? 1)", true),
      ("(odd? 101)", true),
      // (even? n)
      ("(even? -1)", false),
      ("(even? -2)", true),
      ("(even? 0)", true),
      ("(even? 1)", false),
      // (max x1 x2 ...)
      ("(max 1 2 3 4 5 4 3 2 1)", 5),
      ("(max 1)", 1),
      ("(max 3 4)", 4),
      // (min x1 x2 ...)
      ("(min 1)", 1),
      ("(min 3 4)", 3),
      ("(min 3 4.9)", 3),
      ("(min 5 4 3 2 1 2 3 4 5)", 1),
      // (+ z1 ...)
      ("(+ 3 4)", 7),
      ("(+ 3)", 3),
      ("(+)", 0),
      // (* z1 ...)
      ("(* 3 4)", 12),
      ("(* 4)", 4),
      ("(*)", 1),
      // (- z1 z2 ...)
      ("(- 3 4 5)", -6),
      ("(- 3 4)", -1),
      ("(- 3)", -3),
      // (/ z1 z2 ...) No support for fractions
      ("(/ 1 1.0)", 1.0),
      ("(/ 1 2)", 0.5),
      ("(/ 1.0 1)", 1.0),
      ("(/ 4 2)", 2),
      ("(/ 4 2.0)", 2.0),
      // (abs x)
      ("(abs -7)", 7),
      ("(abs 0)", 0),
      ("(abs 7)", 7),
      // (quotient n1 n2)
      ("(quotient -6 2)", -3),
      ("(quotient 3 5)", 0),
      ("(quotient 4 2)", 2),
      // (remainder n1 n2)
      ("(remainder -13 -4)", -1),
      ("(remainder -13 4)", -1),
      ("(remainder 13 -4)", 1),
      ("(remainder 13 4)", 1),
      // (modulo n1 n2)
      ("(modulo -13 -4)", -1),
      ("(modulo -13 4)", 3),
      ("(modulo 13 -4)", -3),
      ("(modulo 13 4)", 1),
      // (gcd n1 ...)
      ("(gcd 32 34)", 2),
      // ("(gcd 32 -36)", 4), // not supported
      // ("(gcd 32)", 32), // not supported
      // ("(gcd)", 0), // not supported
      // (lcm n1 ...)
      ("(lcm 32 34)", 544),
      // ("(lcm 32 -36)", 288), // not supported
      // ("(lcm 32)", 32), // not supported
      // ("(lcm)", 1), // not supported
      // (numerator q) not supported
      // (denominator q) not supported
      // (floor x)
      ("(floor -4.3)", -5.0),
      ("(floor 3.5)", 3.0),
      ("(floor 7)", 7),
      // (ceiling x)
      ("(ceiling -4.3)", -4.0),
      ("(ceiling 3.5)", 4.0),
      // (truncate x) not supported
      // (round x)
      ("(round -4.3)", -4.0),
      ("(round 3.5)", 4.0),
      // ("(round 7/2)", 4), // fractions not supported
      ("(round 7)", 7),
      // (rationalize x y) not supported

      // (exp z) not supported
      // (log z)
      ("(log 1)", 0.0),
      // (sin z)
      ("(sin 0)", 0.0),
      // (cos z)
      ("(cos 0)", 1.0),
      // (tan z)
      ("(tan 0)", 0.0),
      // (asin z)
      ("(asin 0)", 0.0),
      // (acos z)
      ("(acos 1)", 0.0),
      // (atan z)
      ("(atan 0)", 0.0),
      // (atan y x) not supported
      // (sqrt z)
      ("(sqrt 0)", 0),
      ("(sqrt 16)", 4),
      ("(sqrt 4)", 2),
      ("(sqrt 4.0)", 2.0),
      // (expt z1 z2)
      ("(expt 0 0)", 1),
      ("(expt 1 0)", 1),
      ("(expt 5 2)", 25),
      // (make-rectangular x1 x2) not supported
      // (make-polar x3 x4) not supported
      // (real-part z) not supported
      // (imag-part z) not supported
      // (magnitude z) not supported
      // (angle z) not supported
      // (exact->inexact z)
      ("(exact->inexact 0)", 0.0),
      ("(exact->inexact 5)", 5.0),
      // (inexact->exact z)
      ("(inexact->exact 0.000)", 0),
      ("(inexact->exact 5.0)", 5),
      // §6.2.6 Numerical input and output
      // (number->string z)
      ("(equal? (number->string -123.456) \"-123.456\")", true),
      ("(equal? (number->string .5) \"0.5\")", true),
      ("(equal? (number->string 0) \"0\")", true),
      // (number->string z radix) not supported
      // (string->number string)
      ("(string->number \"100\")", 100),
      // ("(string->number \"1e2\")", 100.0), // not supported
      // ("(string->number \"15##\"), 1500.0), // not supported
      // (string->number string radix) not supported

      // §6.3.1 Booleans
      // (not obj)
      ("(not #t)", false),
      ("(not 3)", false),
      ("(not (list 3))", false),
      ("(not #f)", true),
      ("(not '())", false),
      ("(not (list))", false),
      ("(not 'nil)", false),
      // (boolean? obj)
      ("(boolean? #f)", true),
      ("(boolean? '())", false),
      ("(boolean? 0)", false),
      // §6.3.2 Pais and lists
      // (pair? obj)
      ("(pair? (cons 'a 'b))", true),
      ("(pair? '(a b c))", true),
      ("(pair? '())", false),
      ("(pair? (vector 'a 'b))", false),
      // (cons obj1 obj2)
      ("(equal? (cons 'a '()) '(a))", true),
      ("(equal? (cons '(a) '(b c d)) '((a) b c d))", true),
      ("(equal? (cons \"a\" '(b c d)) '(\"a\" b c d))", true),
      // (car pair)
      ("(car '(a b c))", Symbol("a")),
      ("(equal? (car '((a) b c d)) '(a))", true),
      ("(equal? (car (cons 1 2)) 1)", true),
      // (cdr pair)
      ("(equal? (cdr '((a) b c d)) '(b c d))", true),
      ("(equal? (cdr (cons 1 2)) 2)", true),
      // (set-car! pair obj) supported but no test yet
      // (set-cdr! pair obj) supported but no test yet
      // (caar pair) supported but no test yet
      // (cadr pair) supported but no test yet
      // ... all until (cddddr pair) supported but no test yet
      // (null? obj)
      ("(null? '())", true),
      ("(null? '(1 2 3))", false),
      ("(null? (list))", true),
      // (list? obj)
      ("(list? '())", true),
      ("(list? '(1 2 3))", true),
      ("(list? (list))", true),
      ("(list? 1)", false),
      ("(list? (cons 'a 'b))", false),
      ("(list? (cons (list) (list)))", true),
      ("(list? 'a)", false),
      // (list obj ...)
      ("(equal? (list 'a (+ 3 4) 'c) '(a 7 c))", true),
      // (length list)
      ("(length '(a b c))", 3),
      ("(length '(a (b) (c d e)))", 3),
      ("(length '())", 0),
      // (append list ...)
      ("(equal? (append '(x) '(y)) '(x y))", true),
      ("(equal? (append '(a) '(b c d)) '(a b c d))", true),
      ("(equal? (append '(a (b)) '((c))) '(a (b) (c)))", true),
      ("(equal? (append '(a b) (cons 'c 'd)) (cons 'a (cons 'b (cons 'c 'd))))", true),
      ("(equal? (append '() 'a) 'a)", true),
      // (reverse list)
      ("(equal? (reverse '(a b c)) '(c b a))", true),
      ("(equal? (reverse '(a (b c) d (e (f)))) '((e (f)) d (b c) a))", true),
      // (list-tail list k)
      ("(equal? (list-tail '(a b c) 2) '(c))", true),
      // (list-ref list k)
      ("(list-ref '(a b c d) 2)", Symbol("c")),
      ("(list-ref '(a b c d) (inexact->exact (round 1.8)))", Symbol("c")),
      // (memq obj list)
      ("(equal? (memq 'a '(a b c)) '(a b c))", true),
      ("(equal? (memq 'b '(a b c)) '(b c))", true),
      ("(memq 'a '(b c d))", false),
      ("(memq (list 'a) '(b (a) c))", false),
      // (memv obj list)
      ("(equal? (memv 'a '(a b c)) '(a b c))", true),
      ("(equal? (memv 'b '(a b c)) '(b c))", true),
      ("(memv 'a '(b c d))", false),
      ("(memv (list 'a) '(b (a) c))", false),
      ("(equal? (memv 101 '(100 101 102)) '(101 102))", true),
      // (member obj list)
      ("(equal? (member (list 'a) '(b (a) c)) '((a) c))", true),
      // (assq obj alist)
      ("(equal? (assq 'a '((a 1) (b 2) (c 3))) '(a 1))", true),
      ("(equal? (assq 'b '((a 1) (b 2) (c 3))) '(b 2))", true),
      ("(equal? (assq 'c '((a 1) (b 2) (c 3))) '(c 3))", true),
      ("(assq 'd '((a 1) (b 2) (c 3)))", false),
      ("(assq (list 'a) '(((a)) ((b)) ((c))))", false),
      // (assv obj alist)
      ("(equal? (assv 5 '((2 3) (5 7) (11 13))) '(5 7))", true),
      // (assoc obj alist)
      ("(equal? (assoc (list 'a) '(((a)) ((b)) ((c)))) '((a)))", true),
      // §6.3.3 Symbols
      // (symbol? obj)
      ("(symbol? #f)", false),
      ("(symbol? '())", false),
      ("(symbol? 'foo)", true),
      ("(symbol? 'nil)", true),
      ("(symbol? (car '(a b)))", true),
      ("(symbol? \"bar\")", false),
      // (symbol->string symbol)
      ("(equal? (symbol->string 'flying-fish) \"flying-fish\")", true),
      // (string->symbol string)
      ("(string->symbol \"flying-fish\")", Symbol("flying-fish")),
      // §6.3.4 Characters
      // (char? obj)
      ("(char? #\\a)", true),
      ("(char? 1)", false),
      // (char=? char1 char2)
      ("(char=? #\\a #\\a)", true),
      ("(char=? #\\a #\\b)", false),
      ("(char=? #\\b #\\a)", false),
      // (char<? char1 char2)
      ("(char<? #\\a #\\a)", false),
      ("(char<? #\\a #\\b)", true),
      ("(char<? #\\b #\\a)", false),
      // (char>? char1 char2)
      ("(char>? #\\a #\\a)", false),
      ("(char>? #\\a #\\b)", false),
      ("(char>? #\\b #\\a)", true),
      // (char<=? char1 char2)
      ("(char<=? #\\a #\\a)", true),
      ("(char<=? #\\a #\\b)", true),
      ("(char<=? #\\b #\\a)", false),
      // (char>=? char1 char2)
      ("(char>=? #\\a #\\a)", true),
      ("(char>=? #\\a #\\b)", false),
      ("(char>=? #\\b #\\a)", true),
      // (char-ci=? char1 char2)
      ("(char-ci=? #\\A #\\a)", true),
      ("(char-ci=? #\\A #\\b)", false),
      ("(char-ci=? #\\B #\\a)", false),
      // (char-ci<? char1 char2)
      ("(char-ci<? #\\A #\\a)", false),
      ("(char-ci<? #\\A #\\b)", true),
      ("(char-ci<? #\\B #\\a)", false),
      // (char-ci>? char1 char2)
      ("(char-ci>? #\\A #\\a)", false),
      ("(char-ci>? #\\A #\\b)", false),
      ("(char-ci>? #\\B #\\a)", true),
      // (char-ci<=? char1 char2)
      ("(char-ci<=? #\\A #\\a)", true),
      ("(char-ci<=? #\\A #\\b)", true),
      ("(char-ci<=? #\\B #\\a)", false),
      // (char-ci>=? char1 char2)
      ("(char-ci>=? #\\A #\\a)", true),
      ("(char-ci>=? #\\A #\\b)", false),
      ("(char-ci>=? #\\B #\\a)", true),
      // (char-alphabetic? char)
      ("(char-alphabetic? #\\A)", true),
      ("(char-alphabetic? #\\a)", true),
      ("(char-alphabetic? #\\z)", true),
      ("(char-alphabetic? #\\Z)", true),
      ("(char-alphabetic? #\\[)", false),
      ("(char-alphabetic? #\\0)", false),
      // (char-numeric? char) TODO: supported by the abstractinterpreter but not by the concrete one
      // ("(char-numeric? #\\0)", true),
      // ("(char-numeric? #\\9)", true),
      // ("(char-numeric? #\\/)", false),
      // ("(char-numeric? #\\@)", false),
      // (char-whitespace? char) not supported (TODO[easy])
      // (char-upper-case? letter) not supported (TODO[easy])
      // (char-lower-case? letter) not supported (TODO[easy])
      // (char->integer char)
      ("(char->integer #\\a)", 97),
      // (integer->char n)
      ("(integer->char 97)", 'a'),
      // (char-upcase char) TODO: supported by the abstract interpreter but not by the concrete one
      // (char-downcase char) TODO: supported by the abstract interpreter but not by the concrete one

      // §6.3.5. Strings
      // (string? obj)
      ("(string? 'foo)", false),
      ("(string? 1)", false),
      ("(string? \"\")", true),
      ("(string? \"foo\")", true),
      // (make-string k)
      ("(string-length (make-string 5))", 5), // The content of the string is unspecified
      // (make-string k char)
      ("(equal? (make-string 5 #\\c) \"ccccc\")", true),
      // (string char ...) not supported
      // (string-length string)
      ("(string-length \"foobar\")", 6),
      // (string-ref string k)
      ("(string-ref \"foo\" 0)", 'f'),
      // (string-set! string k char) not supported
      // (string=? string1 string2) TODO supported by the abstract but not the concrete interpreter
      // (string-ci=? string1 string2) TODO supported by the abstract but not the concrete interpreter
      // (string<? string1 string2)
      ("(string<? \"bar\" \"foo\")", true),
      ("(string<? \"f\" \"foo\")", true),
      ("(string<? \"foo\" \"bar\")", false),
      ("(string<? \"foo\" \"foo\")", false),
      // (string>? string1 string2)
      ("(string>? \"bar\" \"foo\")", false),
      ("(string>? \"f\" \"foo\")", false),
      ("(string>? \"foo\" \"bar\")", true),
      ("(string>? \"foo\" \"foo\")", false),
      // (string-ci<? string1 string2) not supported
      //("(string-ci<? \"f\" \"foo\")", true),
      //("(string-ci<? \"f\" \"Foo\")", false),
      //("(string-ci<? \"foo\" \"foo\")", false),
      //("(string-ci<? \"foo\" \"Foo\")", false),
      //("(string-ci<? \"Foo\" \"foo\")", true),
      // (string-ci>? string1 string2) not supported
      //("(string-ci>? \"f\" \"foo\")", false),
      //("(string-ci>? \"f\" \"Foo\")", true),
      //("(string-ci>? \"foo\" \"foo\")", false),
      //("(string-ci>? \"foo\" \"Foo\")", true),
      //("(string-ci>? \"Foo\" \"foo\")", false),
      // (string-ci<=? string1 string2) not supported
      // (string-ci>=? string1 string2) not supported

      // (substring string start end)
      ("(equal? (substring \"foo\" 0 3) \"foo\")", true),
      ("(equal? (substring \"foo\" 0 2) \"fo\")", true),
      ("(equal? (substring \"foo\" 1 2) \"o\")", true),
      ("(equal? (substring \"foo\" 0 0) \"\")", true),
      // (string-append string ...)
      ("(equal? (string-append \"foo\" \"bar\") \"foobar\")", true),
      // (string->list string)
      ("(equal? (string->list \"foo\") '(#\\f #\\o #\\o))", true),
      // (list->string list)
      ("(equal? (list->string '(#\\f #\\o #\\o)) \"foo\")", true),
      // (string-copy string) not supported
      // (string-fill! string char)
      ("(begin (define x \"\") (string-fill! x #\\a) (equal? x \"\"))", true),
      ("(begin (define x \"foo\") (string-fill! x #\\a) (equal? x \"aaa\"))", true),
      // §6.3.6 Vectors
      // (vector? obj)
      ("(vector? 'a)", false),
      ("(vector? (vector 'a 'b 'c))", true),
      // (make-vector k)
      // (make-vector k fill) TODO supported but not tested
      // (vector obj ...) TODO supported but not tested
      // (vector-length vector)
      ("(vector-length (vector 0 1 0))", 3),
      ("(vector-length (vector))", 0),
      // (vector-ref vector k)
      ("(vector-ref (vector 'a 'b 'c) 1)", Symbol("b")),
      // (vector-set! vector k obj) TODO supported but not tested
      // (vector->list vector)
      ("(equal? (vector->list (vector 'a 'b 'c)) '(a b c))", true),
      // (list->vector list)
      ("(equal? (list->vector '(a b c)) (vector 'a 'b 'c))", true),
      // (vector-fill! vector fill) not supported

      // §6.4 Control features
      // (procedure? obj)
      ("(procedure? 1)", false),
      ("(procedure? (lambda (x) x))", true),
      // (apply proc1 arg1 ... args)
      ("(apply + (list 3 4))", 7),
      // (map proc list1 list2 ...)
      // ("(equal? (map cadr '((a b) (d e) (g h)) '(b e h)))", true), // TODO: test fails
      // (for-each proc list1 list2 ...)
      ("(equal? (let ((v (make-vector 5))) (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4)) v) (vector 0 1 4 9 16))", true)

      // (force promise) not supported
      // (call-with-current-continuation proc) TODO: supported but not tested

      // (values obj ...) not supported
      // (call-with-values producer consumer) not supported
      // (dynamic-wind before thunk after) not supported
      // (eval expression environment-specifier) not supported
      // (scheme-report-environment version) not supported
      // (null-environment version) not supported
      // (interaction-environment) not supported

      // §6.6 Input and output
      // Section not supported.
    )
