package maf.test.TurgutsThesis.gtr

import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeIf, SchemeParser, SchemeValue}
import maf.core.NoCodeIdentity
import org.scalatest.flatspec.AnyFlatSpec
import maf.language.sexp.Value.Real

class ReplaceTest extends AnyFlatSpec {
  "A schemeExp" should "be able to replace a subexpression by a replacement, and replace it back" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (+ 5 3) (/ 10 2))").head.asInstanceOf[SchemeFuncall]
    val operator: SchemeExp = t.f
    val replacement1: SchemeIf = SchemeIf(operator, operator, operator, NoCodeIdentity)
    val newTree1: SchemeFuncall = t.replace(operator, replacement1).asInstanceOf[SchemeFuncall] //replace
    assert(newTree1.f === replacement1)
    val newTree2: SchemeFuncall = newTree1.replace(replacement1, operator).asInstanceOf[SchemeFuncall] //replace it back
    assert(newTree2.f === operator)
  }

  "A schemeExp" should "be able to replace a deeply nested subexpression by a replacement" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (+ 5 3) (/ 10 2))").head.asInstanceOf[SchemeFuncall]
    val deepArg = t.args.head.asInstanceOf[SchemeFuncall].args.head //corresponds to 5
    val replacement = SchemeValue(Real(999), NoCodeIdentity)
    val newTree: SchemeFuncall = t.replace(deepArg, replacement).asInstanceOf[SchemeFuncall]
    assert(newTree.args.head.asInstanceOf[SchemeFuncall].args.head === replacement)
  }
}
