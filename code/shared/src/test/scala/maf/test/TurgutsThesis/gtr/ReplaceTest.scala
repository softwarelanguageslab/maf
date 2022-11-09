package maf.test.TurgutsThesis.gtr

import maf.core.NoCodeIdentity
import maf.language.scheme.*
import maf.language.sexp.Value.Real
import org.scalatest.flatspec.AnyFlatSpec

class ReplaceTest extends AnyFlatSpec {
  "A schemeExp" should "be able to replace a subexpression by a replacement, and replace it back" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (+ 5 3) (/ 10 2))").head.asInstanceOf[SchemeFuncall]
    t.setPaths()
    val operator: SchemeExp = t.f
    val replacement1: SchemeIf = SchemeIf(operator, operator, operator, NoCodeIdentity)
    val newTree1: SchemeFuncall = t.replace(operator.path, replacement1).asInstanceOf[SchemeFuncall] //replace
    assert(newTree1.f eqlAndPathEql replacement1)

    val newTree2: SchemeFuncall = newTree1.replace(replacement1.path, operator).asInstanceOf[SchemeFuncall] //replace it back
    assert(newTree2.f eqlAndPathEql operator)
  }

  "A schemeExp" should "be able to replace a deeply nested subexpression by a replacement" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (+ 5 3) (/ 10 2))").head.asInstanceOf[SchemeFuncall]
    t.setPaths()

    val deepArg = t.args.head.asInstanceOf[SchemeFuncall].args.head //corresponds to 5
    val replacement = SchemeValue(Real(999), NoCodeIdentity)
    val newTree: SchemeFuncall = t.replace(deepArg.path, replacement).asInstanceOf[SchemeFuncall]
    assert(newTree.args.head.asInstanceOf[SchemeFuncall].args.head eqlAndPathEql replacement)
  }


  "A SchemeExp" should "perform a no-op if no subExpression exists with that path" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (+ 5 3) (/ 10 2))").head.asInstanceOf[SchemeFuncall]
    t.setPaths()

    val replacement = SchemeValue(Real(999), NoCodeIdentity)
    val newTree: SchemeFuncall = t.replace(List(9, 9, 9, 9), replacement).asInstanceOf[SchemeFuncall]

    assert(t eqlAndPathEql newTree)
  }
}
