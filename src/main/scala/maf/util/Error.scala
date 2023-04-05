package maf.util

import maf.syntax.*
import maf.syntax.scheme.*
import cats.Show

/** An error that is not thrown but rather used as an erroneous value. */
// TODO Perhaps rename to ErrorValue?
trait Error extends Serializable
case class ArityError(
    fpos: Identity,
    expected: Int,
    got: Int,
    call: Identity = Identity.none)
    extends Error
case class VarArityError(call: Identity, expected: Int, got: Int) extends Error
case class ValueNotApplicable[V](value: V, idn: Identity) extends Error
//case class NotSupported(message: String)                                  extends Error
case class OperatorNotApplicable[V](operator: String, arguments: List[V]) extends Error
case class TypeError[V](message: String, on: V) extends Error
case class InvalidRelease[V](message: String, on: V) extends Error
case class UndefinedVariableError(id: Identity) extends Error:
    override def toString: String = s"$id"
case class UninitialisedVariableError(id: Identifier[SchemeExp]) extends Error
case class PrimitiveError(errs: Set[Error]) extends Error
case class UndefinedExpressionExpected(idn: Identity) extends Error
case class UnsupportedExpression(exp: Expression[SchemeExp]) extends Error
case object OutOfBoundsError extends Error

/** Use with care, can be used for legacy string based errors */
case class StringError(messsage: String) extends Error
implicit def stringError(message: String): Error = StringError(message)

/** An error that is thrown as an exception. */
trait MAFException extends Throwable

object Error:
    given Show[Error] with
        def show(e: Error): String = e.toString
