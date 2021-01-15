package maf.modular.scheme.modf

import maf.language.scheme._
import maf.modular.scheme.modf.EvalM._

trait SchemeAssertSemantics extends BigStepModFSemantics {
  var assertionsFailed: Set[(Component, SchemeExp)] = Set.empty

  protected def assertViolated(component: Component, exp: SchemeExp): Unit = {
    assertionsFailed = assertionsFailed + ((component, exp))
    assertionsVerified = assertionsVerified - ((component, exp))
  }

  var assertionsVerified: Set[(Component, SchemeExp)] = Set.empty

  protected def assertVerified(component: Component, exp: SchemeExp): Unit = {
    assertionsVerified = assertionsVerified + ((component, exp))
    assertionsFailed = assertionsFailed - ((component, exp))
  }

  def printAssertions(): Unit = {
    println(s"${assertionsVerified.size} assertions were verified, ${assertionsFailed.size} assertions could not be verified.")
    assertionsVerified.foreach({ case (cmp, a) => println(s"Verified $a ($cmp)") })
    assertionsFailed.foreach({ case (cmp, a) => println(s"Failed $a ($cmp)") })
  }

  override def intraAnalysis(cmp: Component): AssertionModFIntra

  trait AssertionModFIntra extends IntraAnalysis with BigStepModFIntra {
    override def evalAssert(exp: SchemeExp): EvalM[Value] =
      for {
        v <- eval(exp)
        res <- merge(
          guard(lattice.isTrue(v)).map { _ =>
            assertVerified(component, exp)
            lattice.void
          },
          guard(lattice.isFalse(v)).map { _ =>
            assertViolated(component, exp)
            lattice.void
          }
        )
      } yield res
  }
}
