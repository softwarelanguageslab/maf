package maf.domain.scheme

import maf.domain.*
import maf.syntax.*
import maf.syntax.scheme.*

/**
 * Addresses to be used with a Scheme analysis.
 * @tparam Context
 *   A type of Contexts that can be used to distinguish more addresses. The type context is covariant.
 */
trait SchemeAddr[+Context] extends Address:
    protected def showCtx(ctx: Any) =
        if (ctx == ()) then "" else s"[$ctx]"
case class VarAddr[Context](id: Identifier, ctx: Context) extends SchemeAddr[Context]:
    def printable = !id.name.startsWith("__")
    def pos: Option[Position] = id.pos
    override def toString: String = s"${id.fullString}${showCtx(ctx)}"
case class PtrAddr[Context](exp: SchemeExp, ctx: Context) extends SchemeAddr[Context]:
    def printable = false
    def pos: Option[Position] = exp.pos
    override def toString: String = s"PtrAddr(${exp.pos.show})${showCtx(ctx)}"

case class PrmAddr(nam: String) extends SchemeAddr[Nothing]:
    def printable = false
    def pos: Option[Position] = None
    override def toString: String = s"PrmAddr($nam)"
