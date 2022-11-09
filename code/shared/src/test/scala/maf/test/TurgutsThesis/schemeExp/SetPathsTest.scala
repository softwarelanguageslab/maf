package maf.test.TurgutsThesis.schemeExp

import maf.language.scheme.{SchemeFuncall, SchemeIf, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class SetPathsTest extends AnyFlatSpec {
  "A SchemeExp" should "be able to set the paths of its descendants" in {

    val t: SchemeFuncall =
      SchemeParser.parseProgramText(
        "(* (if #t 5 3) (begin 5 10))"
      ).head.asInstanceOf[SchemeFuncall]

    assert(t.path equals List())

    t.setPaths()

    assert(t.path equals List())

    assert(t.f.path equals List(0))

    assert(t.args.head.asInstanceOf[SchemeIf].cond.path equals List(1, 0))
  }
}
