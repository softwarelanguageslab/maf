package maf.test.TurgutsThesis.schemeExp

import maf.core.NoCodeIdentity
import maf.language.scheme.*
import maf.language.sexp.Value.Integer
import org.scalatest.flatspec.AnyFlatSpec

class ReplaceTest extends AnyFlatSpec {
  "A schemeExp" should "be able to replace a subexpression by a replacement, and replace it back" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (+ 5 3) (/ 10 2))").head.asInstanceOf[SchemeFuncall]
    val operator: SchemeExp = t.f
    val replacement1: SchemeIf = SchemeIf(operator, operator, operator, NoCodeIdentity)
    val newTree1: SchemeFuncall = t.replace(operator, replacement1).asInstanceOf[SchemeFuncall] //replace
    assert(newTree1.f eq replacement1)

    val newTree2: SchemeFuncall = newTree1.replace(replacement1, operator).asInstanceOf[SchemeFuncall] //replace it back
    assert(newTree2.f eq operator)
  }

  "A schemeExp" should "be able to replace a deeply nested subexpression by a replacement" in {
    var t: SchemeFuncall = SchemeParser.parseProgramText("(* (+ 5 5) (/ 10 2))").head.asInstanceOf[SchemeFuncall]

    t = t.map(subExp => {
      subExp match
        case SchemeValue(value, idn) => SchemeValue(value, NoCodeIdentity)
        case _ => subExp
    }).asInstanceOf[SchemeFuncall]

    val deepArg = t.args.head.asInstanceOf[SchemeFuncall].args.head //corresponds to 5
    val replacement = SchemeValue(Integer(999), NoCodeIdentity)
    val newTree: SchemeFuncall = t.replace(deepArg, replacement).asInstanceOf[SchemeFuncall]
    assert(newTree.args.head.asInstanceOf[SchemeFuncall].args.head eq replacement)
    assert(!(newTree.args.head.asInstanceOf[SchemeFuncall].args(1) eq replacement))
  }
}
