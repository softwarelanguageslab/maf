package maf.modular.scheme.modf

import maf.language.scheme._

trait SchemeAssertSemantics extends BigStepModFSemantics:
    import evalM.*
    var assertionsFailed: Set[(Component, SchemeExp)] = Set.empty

    protected def assertViolated(component: Component, exp: SchemeExp): Unit =
        assertionsFailed = assertionsFailed + ((component, exp))
        assertionsVerified = assertionsVerified - ((component, exp))

    var assertionsVerified: Set[(Component, SchemeExp)] = Set.empty

    protected def assertVerified(component: Component, exp: SchemeExp): Unit =
        assertionsVerified = assertionsVerified + ((component, exp))
        assertionsFailed = assertionsFailed - ((component, exp))

    def printAssertions(): Unit =
        (assertionsVerified.size, assertionsFailed.size) match // Not the prettiest solution but it works.
            case (1, 1) => println(s"1 assertion was verified, 1 assertion could not be verified.")
            case (1, n) => println(s"1 assertion was verified, $n assertions could not be verified.")
            case (m, 1) => println(s"$m assertions were verified, 1 assertion could not be verified.")
            case (m, n) => println(s"$m assertions were verified, $n assertions could not be verified.")
        assertionsVerified.foreach({ case (cmp, a) => println(s"Verified $a ($cmp)") })
        assertionsFailed.foreach({ case (cmp, a) => println(s"Failed $a ($cmp)") })

    override def intraAnalysis(cmp: Component): AssertionModFIntra

    trait AssertionModFIntra extends IntraAnalysis with BigStepModFIntra:
        override def evalAssert(exp: SchemeExp): EvalM[Value] =
          for
              v <- eval(exp)
              res <- merge(
                // We are conservative: we only consider an assertion verified if it is certainly only true. (Putting this tests here avoids a dependency on the order in which assertViolated and assertVerified are called.Ò
                guard(lattice.isTrue(v) && !lattice.isFalse(v)).map { _ =>
                    assertVerified(component, exp)
                    lattice.void
                },
                guard(lattice.isFalse(v)).map { _ =>
                    assertViolated(component, exp)
                    lattice.void
                }
              )
          yield res

    override def configString(): String = super.configString() + "\n  with assertion checking enabled"
