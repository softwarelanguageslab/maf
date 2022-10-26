package maf.test.TurgutsThesis.gtr

import maf.core.NoCodeIdentity
import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeIf, SchemeLettishExp, SchemeParser, SchemeValue}
import maf.language.sexp.Value.Real
import org.scalatest.flatspec.AnyFlatSpec
import maf.TurgutsThesis.gtr.transformations.deleteChildLettishExp
import maf.language.scheme.SchemeLet

class deleteChildTest extends AnyFlatSpec {
  "GTR" should "be able to replace a lets binding and all references to it" in {
    val programText: String =
      "(let " +
        "((x 5))" +
        "(x and #t) (+ x x) (x x x))"

    val letExp: SchemeLet = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLet]
    val res = deleteChildLettishExp(letExp, SchemeLet.apply)
    println(res)
  }
}
