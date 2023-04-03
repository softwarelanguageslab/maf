package maf
package analysis
package primitives

import store.*
import interpreter.*
import syntax.*
import syntax.scheme.*
import util.*
import values.*
import values.scheme.{*, given}
import values.cscheme.*
import values.typeclasses.*
import cats.data.*
import cats.{MonadError => _, *}
import cats.extensions.*
import cats.syntax.all._
import maf.interpreter.SimpleSchemeValue

/** Exception that is thrown when an address is not found in the store. *
  * @param adr
  *   the address that could not be found
  * @note
  *   This exception is considered to be an **analysis error** rather than an error in the program under analysis as the
  *   analysis allocates addresses, not the program under analysis.
  */
case class AddressLookupError(adr: Address) extends MAFException

/** Monad that encodes interactions with the store that contains address `A` and values `V`
  */
trait StoreM[M[_]: Monad, A <: StoreAddress, V]:
  def addrEq: M[MaybeEq[A]]

  /** Find the given address in the store, fails with an exception if the address is not in the store.
    *
    * @throws AddressLookupError
    *   if the address is not found.
    */
  def lookupSto[A <: StoreAddress](adr: A): M[adr.Value]

  /** Extend the store on the given address with the given value. */
  def extendSto[A <: StoreAddress](a: A, v: a.Value): M[Unit]

  /** Update the store on the given address with the given value.
    *
    * @note
    *   used in the context of destructively updating an existing value in the store, whereas `extendSto` is used for
    *   introducing new bindings in the store.
    */
  def updateSto(a: A, v: V): M[Unit]

  // let Scala know to use the instance itself as an implicit
  implicit final private val self: StoreM[M, A, V] = this

/** Monad encoding allocations of new addresses */
trait AllocM[M[_]: Monad, Vec, Pai, A]:
  /** Allocates a new address based on a source code identifier, usually associated with variables
    */
  def allocVar(idn: Identifier[SchemeExp]): M[A]

  /** Allocates a new address based on an expression in the source code, usually associated with heap-allocated values
    */
  def allocVec(exp: SchemeExp): M[VectorAddress[Vec]]
  def allocPai(exp: SchemeExp): M[PairAddress[Pai]]

/** Monadic operations for implementing Scheme primitives */
trait SchemePrimM[M[_]: Monad, V, Vec, Pai, A <: Address]
    extends MonadJoin[M]
    with cats.MonadError[M, Error]
    with AllocM[M, Vec, Pai, A]
    with StoreM[M, A, V]:

  // for convenience
  def storePai(exp: SchemeExp, vlu: Pai): M[PairAddress[Pai]] =
    allocPai(exp).flatMap(adr => extendSto[PairAddress[Pai]](adr, vlu) >> adr.pure)
  def storeVec(exp: SchemeExp, vlu: Vec): M[VectorAddress[Vec]] =
    allocVec(exp).flatMap(adr => extendSto[VectorAddress[Vec]](adr, vlu) >> adr.pure)

  def deref[X: Lattice, A <: StoreAddress](adr: A)(f: (adr.Value) => M[X]): M[X] =
    lookupSto(adr) >>= f

  def allocList(
      exs: List[SchemeExp],
      vlus: List[V]
    )(using
      dom: SchemeDomain[V, Pai, Vec]
    ): M[V] =
    exs.zip(vlus).foldM[M, V](dom.lattice.nil) { case (rest, (ex, vlu)) =>
      storePai(ex, dom.pairLattice.cons(vlu, rest)).map(adr => dom.lattice.ptr(adr))
    }

  def currentThread: M[TID] = throw new Exception("Not supported")

  // exotic -- not so important if not implemented yet
  // def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): M[V] =
  //  throw new Exception("Not supported")
