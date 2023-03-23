package maf
package analysis
package primitives

import interpreter.*
import syntax.*
import syntax.scheme.*
import util.*
import values.*
import values.scheme.{given, *}
import values.cscheme.*
import values.typeclasses.*
import cats.data.*
import cats.{MonadError => _, *}
import cats.extensions.*
import cats.syntax.all._

/** Exception that is thrown when an address is not found in the store. *
  * @param adr
  *   the address that could not be found
  * @note
  *   This exception is considered to be an **analysis error** rather than an
  *   error in the program under analysis as the analysis allocates addresses,
  *   not the program under analysis.
  */
case class AddressLookupError(adr: Address) extends MAFException

/** Monad that encodes interactions with the store that contains address `A` and
  * values `V`
  */
trait StoreM[M[_]: Monad, A, V]:
  def addrEq: M[MaybeEq[A]]

  /** Find the given address in the store, fails with an exception if the
    * address is not in the store.
    *
    * @throws AddressLookupError
    *   if the address is not found.
    */
  def lookupSto(a: A): M[V]

  /** Extend the store on the given address with the given value. */
  def extendSto(a: A, v: V): M[Unit]
  def extendSto(bds: Iterable[(A, V)]): M[Unit] =
    bds.toList.traverse_ { case (adr, vlu) => extendSto(adr, vlu) }

  /** Update the store on the given address with the given value.
    *
    * @note
    *   used in the context of destructively updating an existing value in the
    *   store, whereas `extendSto` is used for introducing new bindings in the
    *   store.
    */
  def updateSto(a: A, v: V): M[Unit]

  // let Scala know to use the instance itself as an implicit
  implicit final private val self: StoreM[M, A, V] = this

/** Monad encoding allocations of new addresses */
trait AllocM[M[_]: Monad, A]:
  /** Allocates a new address based on a source code identifier, usually
    * associated with variables
    */
  def allocVar(idn: Identifier[SchemeExp]): M[A]

  /** Allocates a new address based on an expression in the source code, usually
    * associated with heap-allocated values
    */
  def allocPtr(exp: SchemeExp): M[A]

/** Monadic operations for implementing Scheme primitives */
trait SchemePrimM[M[_]: Monad, A <: Address, V]
    extends MonadJoin[M]
    with cats.MonadError[M, Error]
    with AllocM[M, A]
    with StoreM[M, A, V]:

  // for convenience
  def allocVal(exp: SchemeExp, vlu: V): M[A] =
    allocPtr(exp).flatMap(adr => extendSto(adr, vlu) >> adr.pure)

  def deref[X: Lattice](x: Set[A])(f: (A, V) => M[X]): M[X] =
    mfoldMap(x)(a => lookupSto(a) >>= (v => f(a, v)))
  def allocList(exs: List[SchemeExp], vlus: List[V])(using
      lat: SchemeLattice[V]
  ): M[V] =
    exs.zip(vlus).foldM[M, V](SchemeNil) { case (rest, (ex, vlu)) =>
      allocVal(ex, lat.cons(vlu, rest)).map(adr => SchemePtr(adr))
    }
  def currentThread: M[TID] = throw new Exception("Not supported")

  // exotic -- not so important if not implemented yet
  // def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): M[V] =
  //  throw new Exception("Not supported")
