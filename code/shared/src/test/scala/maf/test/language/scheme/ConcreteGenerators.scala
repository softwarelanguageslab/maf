package maf.test.language.scheme

import maf.language.scheme.interpreter.ConcreteValues
import maf.test.lattice.Generators
import maf.core.Identity
import org.scalacheck.Gen

/**
 * A concrete input generator.
 *
 * @param primitives
 *   a list of primitives to choose from in case a primitive is selected
 */
class ConcreteGenerators(primitives: List[String]):
    import ConcreteValues.Value.*

    private type V = ConcreteValues.Value

    protected def str: Gen[Str] = Generators.str.map(Str(_))
    protected def prim: Gen[Primitive] = Gen.oneOf(primitives).map(Primitive(_))
    protected def undefined: Gen[Undefined] = Gen.oneOf(List(Undefined(Identity.none)))
    protected def symbol: Gen[Symbol] = Generators.str.map(Symbol(_))
    protected def integer: Gen[Integer] = Generators.int.map(Integer(_))
    protected def real: Gen[Real] = Generators.double.map(Real(_))
    protected def bool: Gen[Bool] = Gen.oneOf(List(true, false)).map(Bool(_))
    protected def character: Gen[Character] = Generators.char.map(Character(_))
    protected def nil: Gen[Nil.type] = Gen.oneOf(List(Nil))
    protected def cons: Gen[Cons] =
      for
          car <- any
          cdr <- any
      yield Cons(car, cdr)

    /** Generates a random value in the concrete domain */
    def any: Gen[V] = Gen.oneOf(str, prim, undefined, symbol, integer, real, bool, character, nil, cons)
