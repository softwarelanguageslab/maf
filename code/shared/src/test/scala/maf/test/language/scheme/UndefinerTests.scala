package maf.test.language.scheme

import maf.language.scheme.*
import org.scalatest.propspec.AnyPropSpec

class UndefinerTests extends AnyPropSpec:
    // Compares the given programs based on their structure, but ignores idenities
    def comparePrograms(program1: SchemeExp, program2: SchemeExp): Boolean =
      (program1, program2) match
          case (SchemeLambda(name, args, body, ann, _), SchemeLambda(name1, args1, body1, ann1, _))
              if name == name1 && args.zip(args1).forall { case (a, b) => a == b } && ann == ann1 =>
            body.zip(body1).forall { case (a, b) => a == b } // TODO: args also have an identity that should be ignored

          case (SchemeVarArgLambda(name, args, vararg, body, ann, _), SchemeVarArgLambda(name1, args1, vararg1, body1, ann1, _))
              if name == name1 && args.zip(args1).forall { case (a, b) => a == b } && ann == ann1 && vararg == vararg1 =>
            body.zip(body1).forall { case (a, b) => a == b }

          case (SchemeFuncall(f, args, _), SchemeFuncall(f1, args1, _)) =>
            comparePrograms(f, f1) && args.zip(args1).forall { case (a, b) => comparePrograms(a, b) }
          case (SchemeIf(cond, cons, alt, _), SchemeIf(cond1, cons1, alt1, _)) =>
            comparePrograms(cond, cond1) && comparePrograms(cons, cons1) && comparePrograms(alt, alt1)

          case (SchemeLet(bindings, body, _), SchemeLet(bindings1, body1, _)) => ???
//case SchemeLetStar(bindings, body, idn)                             => ???
//case SchemeLetrec(bindings, body, idn)                              => ???
//case SchemeSet(variable, value, idn)                                => ???
//case SchemeSetLex(variable, lexAddr, value, idn)                    => ???
//case SchemeBegin(exps, idn)                                         => ???
//case SchemeDefineVariable(name, value, idn)                         => ???
//case SchemeDefineFunction(name, args, body, idn)                    => ???
//case SchemeDefineVarArgFunction(name, args, vararg, body, idn)      => ???
//case SchemeVar(id)                                                  => ???
//case SchemeVarLex(id, lexAdr)                                       => ???
//case SchemeValue(value, idn)                                        => ???
//
//
    def test(name: String, original: String, translated: String): Unit = () // TODO

    // top level code
    test(
      "top level code",
      """
         |(define x 10)
         |(define y 20)
         |(display y)
         """,
      """
        (begin
           (letrec ((x 10)
                    (y 20))
              (display y)))
         """
    )

    // nested begin's
    test(
      "nested begins",
      """
        |(define x 10)
        |(define y 20)
        |(define (foo)
        |  (begin 
        |    (begin 
        |      (define z 40))
        |    (display z)))
        |(foo)
        """,
      """
        |(begin
        |   (letrec ((x 10)
        |            (y 20)
        |            (foo (lambda ()
        |                   (letrec ((z 40))
        |                      (begin
        |                         (display z))))))
        |      (foo)))
        """
    )

    // if expressions and defines
    test(
      "if expressions and defines",
      """
      | (define x 10)
      | (define y 20)
      | (if (= x 10)
      |     y
      |     x)
      """,
      """
      |(begin
      | (letrec ((x 10)
      |          (y 20))
      |    (if (= x 10) y x)))
      """
    )

    // lets and defines
    test(
      "lets and defines",
      """
      | (define x 10)
      |   (let ((y 20))
      |     (define x 30)
      |     (display x))
      """,
      """
      | (begin
      |  (letrec ((x 10))
      |     (let ((y 20))
      |        (letrec ((x 30))
      |           (display x)))))
      """
    )

    // nested defines
    test(
      "nested defines",
      """
    | (define (fac n)
    | (define (fac-iter x n)
    |   (if (= x 0) n (fac-iter (- x 1) (* n x))))

    | (fac-iter n))
    """,
      """
    |(begin
    | (letrec ((fac (lambda (n)
    |                 (letrec ((fac-iter (lambda (x n)
    |                                      (if (= x 0) n (fac-iter (- x 1) (* n x))))))
    |                    (fac-iter n)))))))
    """
    )

    test(
      "begins with only defines",
      """
      | (begin
      |  (define x 10)
      |  (define y 20))
      """,
      """
      | (begin
      |  (letrec ((x 10)
      |           (y 20))))
      """
    )
