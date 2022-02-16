package maf.core

import maf.core.Position.Position

/** An error that is not thrown but rather used as an erroneous value. */
// TODO Perhaps rename to ErrorValue?
trait Error extends Serializable
case class ArityError(fpos: Position, expected: Int, got: Int, call: Identity = Identity.none) extends Error
case class VarArityError(call: Position, expected: Int, got: Int) extends Error
case class ValueNotApplicable[V](value: V, idn: Identity) extends Error
//case class NotSupported(message: String)                                  extends Error
case class OperatorNotApplicable[V](operator: String, arguments: List[V]) extends Error
case class TypeError[V](message: String, on: V) extends Error
case class InvalidRelease[V](message: String, on: V) extends Error
case class UndefinedVariableError(id: Identifier) extends Error:
    override def toString: String = s"$id:${id.idn.pos}"
case class UninitialisedVariableError(id: Identifier) extends Error
case class PrimitiveError(errs: Set[Error]) extends Error
case class UndefinedExpressionExpected(idn: Identity) extends Error
case class UnsupportedExpression(exp: Expression) extends Error

/** Use with care, can be used for legacy string based errors */
case class StringError(messsage: String) extends Error
implicit def stringError(message: String): Error = StringError(message)

/** An error that is thrown as an exception. */
trait MAFException extends Throwable
