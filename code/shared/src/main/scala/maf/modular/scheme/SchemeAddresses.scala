package maf.modular.scheme

import maf.core.*
import maf.language.scheme.*

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
    def idn: Identity = id.idn
    override def toString: String = s"${id.fullString}${showCtx(ctx)}"
case class PtrAddr[Context](exp: SchemeExp, ctx: Context) extends SchemeAddr[Context]:
    def printable = false
    def idn: Identity = exp.idn
    override def toString: String = s"PtrAddr(${exp.idn.pos})${showCtx(ctx)}"
trait ExceptionAddr[Context] extends SchemeAddr[Context]
case class SrcAddr[Context](nam: Identifier, ctx: Context) extends SchemeAddr[Context]:
    def printable = true
    def idn: Identity = nam.idn
case class SanAddr[Context](nam: Identifier, ctx: Context) extends SchemeAddr[Context]:
    def printable = true
    def idn: Identity = nam.idn
case class SnkAddr[Context](nam: Identifier, ctx: Context) extends SchemeAddr[Context]:
    def printable = true
    def idn: Identity = nam.idn

case class PrmAddr(nam: String) extends SchemeAddr[Nothing]:
    def printable = false
    def idn: Identity = Identity.none
    override def toString: String = s"PrmAddr($nam)"

case class LitAddr[Context](exp: SchemeExp) extends SchemeAddr[Context]:
    def printable = true
    def idn: Identity = exp.idn
    override def toString: String = s"Literal($exp@${exp.idn})"
case class FlowAddr[Component](cmp: Component, e: Option[SchemeExp] = None) extends SchemeAddr[Component]:
    def printable = false
    def idn: Identity = Identity.none
    override def toString: String = e match
        case Some(e) => s"ifCtx($cmp :: ${e.idn.pos})"
        case None => s"cmpCtx($cmp)"
case class PrimFlowAddr[Component](cmp: Component, e: SchemeExp) extends SchemeAddr[Component]:
    def printable = false
    def idn: Identity = Identity.none
    override def toString: String = s"prim($e@${e.idn.pos})"