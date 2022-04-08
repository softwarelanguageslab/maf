package maf.test.language.symbolic

import maf.test.*
import maf.language.scheme.*
import maf.language.symbolic.*
import maf.core.{IdentityMonad, Monad, MonadStateT, StateOps}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class PathConditionTests extends AnyFlatSpec with Matchers:
    import maf.language.symbolic.Symbolic.*
    case class FreshState(n: Int)
    object FreshState:
        import maf.core.Monad.MonadSyntaxOps
        val freshInstance: StateOps[FreshState, [A] =>> MonadStateT[FreshState, IdentityMonad.Id, A]] = MonadStateT.stateInstance

        export MonadStateT.given

        def fresh: FreshMonad[SchemeExp] =
            import freshInstance.*
            for
                st <- get
                st1 = st.copy(n = st.n + 1)
                _ <- put(st1)
            yield VarId(s"x${st.n}")

    type FreshMonad[X] = MonadStateT[FreshState, IdentityMonad.Id, X]
    import FormulaAux.*

    /**
     * A test that tests whether the given initial formula is garbage collected to the given expected formula.
     *
     * @param initial
     *   the formula to start from
     * @param expected
     *   the formula that is expected after garbage collection
     * @param roots
     *   a set of expressions that whose items need to be kept in the path condition
     */
    def shouldGarbageCollectTo(initial: Formula, expected: Formula, roots: Set[Symbolic], vars: Int): Unit =
        s"$initial" should s"garbage collect to $expected" taggedAs (ScvTest, SymbolicTest) in {
            val initialState = FreshState(n = vars)
            val simplified = PathCondition(initial).simplify(roots, PathCondition.onlyVarsAllowed, FreshState.fresh)
            simplified.runValue(initialState)._1.formula shouldEqual expected
        }

    shouldGarbageCollectTo(
      initial = conj(
        ass(isTrue(ap(id(">"), ap(id("-"), id("x0"), num(1)), num(0)))),
        ass(isTrue(ap(id(">"), id("x0"), num(0))))
      ),
      expected = conj(
        ass(isTrue(ap(id(">"), id("x0"), num(0))))
      ),
      roots = Set(ap(id("-"), id("x0"), num(1))),
      vars = 1
    )

    shouldGarbageCollectTo(
      initial = conj(
        ass(isTrue(ap(id(">"), ap(id("-"), id("x1"), num(1)), num(0)))),
        ass(isTrue(ap(id(">"), id("x0"), num(0))))
      ),
      expected = conj(
        ass(isTrue(ap(id(">"), id("x1"), num(0)))),
        ass(isTrue(ap(id(">"), id("x0"), num(0))))
      ),
      roots = Set(ap(id("-"), id("x1"), num(1)), id("x0")),
      vars = 1
    )
