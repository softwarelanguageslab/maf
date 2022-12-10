package maf.test.TurgutsThesis.gtr

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.*
import maf.TurgutsThesis.gtr.transformations.generics.{DeleteChildSimple, ReplaceByChild}
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class GTRTest extends AnyFlatSpec {
  "GTR" should "be able to reduce a program" in {
    val programText: String =
      "(begin " +
        "(if (= x 5) #t #f)" +
        "(begin (+ x 2) (* 100 100) (if #f #f #f)))"

    val t = SchemeParser.parseProgramText(programText).last

    val reduced = GTR.reduce(t,
      t => t.isInstanceOf[SchemeBegin] && t.asInstanceOf[SchemeBegin].exps.length == 1, identity,
      List(ReplaceByChild, DeleteChildSimple))

    assert(reduced.isInstanceOf[SchemeBegin])
    assert(reduced.asInstanceOf[SchemeBegin].exps.length == 1)
    }
}
