package maf.language.scheme.interpreter

import maf.core.Identity
import maf.language.scheme._
import maf.language.sexp.SExp

trait BaseSchemeInterpreter[V] {
  // TODO: Maybe not all these definitions need to be abstract as some can be shared with the CPS interpreter.

  import ConcreteValues._

  def newAddr(meta: AddrInfo): (Int, AddrInfo)

  var store: Map[Addr, Value]

  def extendStore(a: Addr, v: Value): Unit

  def lookupStore(a: Addr): Value

  def lookupStoreOption(a: Addr): Option[Value]

  def setStore(s: Map[Addr, Value]): Unit

  def allocateVal(exp: SchemeExp, value: Value): Value.Pointer

  def allocateCons(
      exp: SchemeExp,
      car: Value,
      cdr: Value
    ): Value

  def allocateStr(exp: SchemeExp, str: String): Value.Pointer

  def getString(addr: Addr): String

  def makeList(values: List[(SchemeExp, Value)]): Value

  val stack: Boolean

  def stackedCall(
      name: Option[String],
      idn: Identity,
      block: => V
    ): V

  def stackedException[R](msg: String): R

  val io: IO

  def evalSExp(sexp: SExp, exp: SchemeExp): ConcreteValues.Value
}
