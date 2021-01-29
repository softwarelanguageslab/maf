package maf.language.contracts.primitives

import maf.language.contracts.ScSchemeLattice
import maf.language.scheme.primitives.SchemePrimitive
import maf.language.contracts.lattices.ScOp
import maf.language.scheme.primitives.SchemeInterpreterBridge
import maf.language.scheme.SchemeExp
import maf.core.MayFail
import maf.core.Store
import maf.core.Address

/** Implementation of the primitives particular to the contracts language */
class ScLatticePrimitives[L, A <: Address]()(implicit val scLattice: ScSchemeLattice[L, A, SchemePrimitive[L, A]]) {
  val allPrimitives: List[ScPrimitive[L, A]] = List(
    `dependent-contract?`,
    `nonzero?`,
    `monitored?`,
    `number?`,
    `bool?`,
    `true?`,
    `false?`,
    `any?`
  )

  abstract class latticeOp(op: ScOp) extends ScPrimitive[L, A] {
    val name = op.name
    override def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, L)],
        store: Store[A, L],
        alloc: SchemeInterpreterBridge[L, A]
      ): MayFail[(L, Store[A, L]), maf.core.Error] =
      scLattice.op(op)(args.map(_._2)).map(v => (v, store))

  }
  case object `dependent-contract?` extends latticeOp(ScOp.IsDependentContract)
  case object `nonzero?` extends latticeOp(ScOp.IsNonZero)
  case object `monitored?` extends latticeOp(ScOp.IsMonitored)
  case object `number?` extends latticeOp(ScOp.IsNumber)
  case object `bool?` extends latticeOp(ScOp.IsBool)
  case object `true?` extends latticeOp(ScOp.IsTrue)
  case object `false?` extends latticeOp(ScOp.IsFalse)
  case object `any?` extends latticeOp(ScOp.IsAny)
}
