package maf.test.deltaDebugging.schemeExp

import maf.language.scheme._
import maf.language.sexp.Value
import org.scalatest.flatspec.AnyFlatSpec

class MapTest extends AnyFlatSpec {
  "A SchemeExp" should "be mappable" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (if #t 5 3) (begin 5 10))").head.asInstanceOf[SchemeFuncall]
    val mapped: SchemeFuncall = t.map(e => {
      e match
        case SchemeIf(cond, cons, alt, idn) => SchemeIf(alt, alt, alt, idn)
        case any => any
    }).asInstanceOf[SchemeFuncall]
    assert(!(mapped eq t)) //mapped should be new tree

    val firstIf = mapped.args.head

    assert(firstIf.toString equals "(if 3 3 3)")

    val beginExp = mapped.args(1)

    assert(beginExp.toString equals "(begin 5 10)")
  }


  "A SchemeExp" should "be deep mappable" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (if #t 5 3) (begin (begin (if #t 2 1))))").head.asInstanceOf[SchemeFuncall]

    val mapped: SchemeFuncall = t.map(e => {
      e match
        case sexp@SchemeValue(v, idn) => v match
          case Value.Integer(bigInt) =>
            SchemeValue(Value.Integer(-bigInt), idn)
          case anyVal => sexp
        case e => e
    }).asInstanceOf[SchemeFuncall]
    assert(!(mapped eq t)) //mapped should be new tree

    val firstIf: SchemeIf = mapped.args.head.asInstanceOf[SchemeIf]
    val firstIfThen: SchemeValue = firstIf.cons.asInstanceOf[SchemeValue]

    assert(firstIfThen.value.asInstanceOf[Value.Integer].value == -5)

    val deepIf: SchemeIf = mapped.args(1).asInstanceOf[SchemeBegin].exps.head.asInstanceOf[SchemeBegin].exps.head.asInstanceOf[SchemeIf]
    val deepIfElse: SchemeValue = deepIf.alt.asInstanceOf[SchemeValue]

    assert(deepIfElse.value.asInstanceOf[Value.Integer].value == -1)
  }
}
