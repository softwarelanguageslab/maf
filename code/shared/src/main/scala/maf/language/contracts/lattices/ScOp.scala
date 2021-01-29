package maf.language.contracts.lattices

import maf.language.scheme.lattices.SchemeOp
import maf.lattice.interfaces.Operation1
import maf.lattice.interfaces.Operation

sealed trait ScOp extends Operation

object ScOp {
  case class ScSchemeOp(op: SchemeOp) extends ScOp {
    val arity: Int = op.arity
    val name: String = op.name
    override def arityException[L](args: List[L]): String =
      op.arityException(args)
  }

  case object IsDependentContract extends Operation1("dependent-contract?") with ScOp
  case object IsNonZero extends Operation1("nonzero?") with ScOp
  case object IsPair extends Operation1("pair?") with ScOp
  case object IsMonitored extends Operation1("monitored?") with ScOp
  case object IsTrue extends Operation1("true?") with ScOp
  case object IsFalse extends Operation1("false?") with ScOp
  case object IsAny extends Operation1("any?") with ScOp

  /**
   * Replacement for `int?`, which checks whether the value is a number (integer or real)
   * or an opaque that is refined as such.
   */
  case object IsNumber extends Operation1("number?") with ScOp

  /** For compability with older sc language programs */
  case object IsBool extends Operation1("bool?") with ScOp
}
