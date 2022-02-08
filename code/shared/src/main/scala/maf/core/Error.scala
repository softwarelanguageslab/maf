package maf.core

import maf.core.Position.Position

/** An error that is not thrown but rather used as an erroneous value. */
// TODO Perhaps rename to ErrorValue?
trait Error extends Serializable
case class ArityError(call: Position, expected: Int, got: Int) extends Error
case class VarArityError(call: Position, expected: Int, got: Int) extends Error
//case class NotSupported(message: String)                                  extends Error
case class OperatorNotApplicable[V](operator: String, arguments: List[V]) extends Error
case class TypeError[V](message: String, on: V) extends Error
case class InvalidRelease[V](message: String, on: V) extends Error
case class UndefinedVariableError(id: Identifier) extends Error
case class PrimitiveError(errs: Set[Error]) extends Error

/** An error that is thrown as an exception. */
trait MAFException extends Throwable
