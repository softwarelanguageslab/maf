package maf.test.language.symbolic

import maf.test.*
import maf.language.symbolic.*
import org.scalatest.flatspec.AnyFlatSpec

/** Tests for testing the implementation of disjunctive normal form */
class DNFTests extends AnyFlatSpec:

    """(real? x0) /\\ (
   ((real? x0) /\\  (>= x0 0)) \\/
   ((= x0 0))
  )""" should "convert to ((real? x0) /\\ (>= x0 0)) \\/ ((real? x0) /\\ (= x0 0))" taggedAs (ScvTest, SymbolicTest) in {
        import FormulaAux.*
        val formulaD = conj(
          conj(
            ass(isTrue(ap(id("real?"), id("x0")))),
            ass(isTrue(ap(id("number?"), num(0)))),
          ),
          conj(
            ass(isTrue(ap(id("real?"), id("x0")))),
            ass(isTrue(ap(id("number?"), num(0)))),
          ),
          disj(
            conj(
              ass(isTrue(ap(id("real?"), id("x0")))),
              ass(isTrue(ap(id(">="), id("x0"), num(0))))
            ),
            ass(isTrue(ap(id("="), id("x0"), num(0))))
          )
        )

        val expected = disj(
          conj(
            ass(isTrue(ap(id("real?"), id("x0")))),
            ass(isTrue(ap(id("number?"), num(0)))),
            ass(isTrue(ap(id(">="), id("x0"), num(0))))
          ),
          conj(
            ass(isTrue(ap(id("real?"), id("x0")))),
            ass(isTrue(ap(id("number?"), num(0)))),
            ass(isTrue(ap(id("="), id("x0"), num(0))))
          )
        )

        assert(DNF.dnf(formulaD) == expected)
    }
