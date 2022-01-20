package maf.test.language.scheme

import maf.language.scheme.*
import org.scalatest.propspec.AnyPropSpec
import maf.core.Identity

class UndefinerTests extends AnyPropSpec:
    // Compares the given programs based on their structure, but ignores idenities
    def comparePrograms(program1: SchemeExp, program2: SchemeExp): Boolean =
      (program1, program2) match
          case (SchemeLambda(name, args, body, ann, _), SchemeLambda(name1, args1, body1, ann1, _))
              if name == name1 && args.zip(args1).forall { case (a, b) => a.name == b.name } && ann == ann1 =>
            body.zip(body1).forall { case (a, b) => a == b } // TODO: args also have an identity that should be ignored

          case (SchemeVarArgLambda(name, args, vararg, body, ann, _), SchemeVarArgLambda(name1, args1, vararg1, body1, ann1, _))
              if name == name1 && args.zip(args1).forall { case (a, b) => a.name == b.name } && ann == ann1 && vararg == vararg1 =>
            body.zip(body1).forall { case (a, b) => a == b }

          case (SchemeFuncall(f, args, _), SchemeFuncall(f1, args1, _)) =>
            comparePrograms(f, f1) && args.zip(args1).forall { case (a, b) => comparePrograms(a, b) }
          case (SchemeIf(cond, cons, alt, _), SchemeIf(cond1, cons1, alt1, _)) =>
            comparePrograms(cond, cond1) && comparePrograms(cons, cons1) && comparePrograms(alt, alt1)

          case (SchemeLet(bindings, body, _), SchemeLet(bindings1, body1, _)) =>
            bindings.map(_._1.name).zip(bindings1.map(_._1.name)).forall { case (a, b) => a == b } && body.zip(body1).forall { case (body, body1) =>
              comparePrograms(body, body1)
            }

          case (SchemeLetStar(bindings, body, _), SchemeLetStar(bindings1, body1, _)) =>
            bindings.map(_._1.name).zip(bindings1.map(_._1.name)).forall { case (a, b) => a == b } && body.zip(body1).forall { case (body, body1) =>
              comparePrograms(body, body1)
            }
          case (SchemeLetrec(bindings, body, _), SchemeLetrec(bindings1, body1, _)) =>
            bindings.map(_._1.name).zip(bindings1.map(_._1.name)).forall { case (a, b) => a == b } && body.zip(body1).forall { case (body, body1) =>
              comparePrograms(body, body1)
            }
          case (SchemeSet(variable, value, _), SchemeSet(variable1, value1, _)) =>
            variable.name == variable1.name && comparePrograms(value, value1)

          case (SchemeSetLex(variable, _, value, _), SchemeSetLex(variable1, _, value1, _)) =>
            variable.name == variable1.name && comparePrograms(value, value1)

          case (SchemeBegin(exps, _), SchemeBegin(exps1, _)) =>
            exps.zip(exps1).forall { case (e1, e2) => comparePrograms(e1, e2) }

          case (SchemeDefineVariable(name, value, _), SchemeDefineVariable(name1, value1, _)) =>
            name.name == name1.name && comparePrograms(value, value1)

          case (SchemeVar(id), SchemeVar(id2))             => id.name == id2.name
          case (SchemeVarLex(id, _), SchemeVarLex(id2, _)) => id.name == id2.name
          case (SchemeValue(value, _), SchemeValue(value1, idn)) =>
            value == value1
          case (e1, e2) => throw new Exception(s"comparison between $e1 and $e2 is not supported")

    def test(name: String, original: String, translated: String): Unit =
      property(name) {
        val parsedOriginal = SchemeParser.parse(original.stripMargin)
        val parsedTranslated = SchemeParser.parse(translated.stripMargin).head
        val undefinedOriginal = SchemeBegin(SchemeMonadicUndefiner.undefineExps(parsedOriginal, false), Identity.none)
        assert(comparePrograms(parsedTranslated, undefinedOriginal))
      }

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
    |   (define (fac-iter x n)
    |     (if (= x 0) n (fac-iter (- x 1) (* n x))))
    |   (fac-iter n))
    | (fac 5)
    """,
      """
    | (begin
    |   (letrec ((fac (lambda (n)
    |     (letrec ((fac-iter (lambda (x n)
    |       (if (= x 0) n (fac-iter (- x 1) (* n x))))))
    |     (fac-iter n)))))
    | (fac 5)))
    """
    )
