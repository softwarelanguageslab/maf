package maf.test.deltaDebugging.schemeExp

import maf.core.NoCodeIdentity
import maf.language.scheme.{SchemeBegin, SchemeExp, SchemeFuncall, SchemeIf, SchemeParser, SchemeValue}
import maf.language.sexp.Value.Real
import org.scalatest.flatspec.AnyFlatSpec

class ParentTest extends AnyFlatSpec {
  "A schemeExp" should "return None as parent if its the root" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (+ 5 3) (/ 10 2))").head.asInstanceOf[SchemeFuncall]
    val parent: Option[SchemeExp] = t.parent(t)

    assert(parent.isEmpty)
  }

  "A SchemeExp" should "be able to find its parent" in {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (if #t 5 3) (begin 5 10))").head.asInstanceOf[SchemeFuncall]
    val child: SchemeBegin = t.args(1).asInstanceOf[SchemeBegin]
    val childOfChild: SchemeExp = child.exps(1)

    val parentOfChild = t.parent(child).get
    assert(parentOfChild eq t)

    val parentOfChildOfChild = t.parent(childOfChild).get
    assert(parentOfChildOfChild eq child)
  }
}
