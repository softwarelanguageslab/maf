package maf.modular.scheme.modf

import maf.language.scheme._
import maf.modular.scheme.modf.EvalM._

trait SchemeAssertSemantics extends BigStepModFSemantics {
  var assertionsFailed: Set[(Component, SchemeExp)] = Set.empty
  protected def assertViolated(component: Component, exp: SchemeExp): Unit =
    assertionsFailed = assertionsFailed + ((component, exp))

  override def intraAnalysis(cmp: Component): AssertionModFIntra
  trait AssertionModFIntra extends IntraAnalysis with BigStepModFIntra {
    override def evalAssert(exp: SchemeExp): EvalM[Value] = {
      for {
        v <- eval(exp)
        res <- merge(guard(lattice.isTrue(v)).map(_ => lattice.void),
          guard(lattice.isFalse(v)).map(_ => {
            assertViolated(component, exp)
            lattice.void
          }))
      } yield res
    }
  }
}
