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
  *   This exception is considered to be an **analysis error** rather than an error in the program under analysis as the analysis allocates addresses,
  *   not the program under analysis.
  */
case class AddressLookupError(adr: Address) extends MAFException

/** Monad that encodes interactions with the store that contains address `A` and values `V`
  */
trait StoreM[M[_]: Monad, A, V]:
    def addrEq: M[MaybeEq[A]]

    /** Find the given address in the store, fails with an exception if the address is not in the store.
      *
      * @throws AddressLookupError
      *   if the address is not found.
      */
    def lookupSto[A <: StoreAddress](adr: A): M[adr.Value]

    /** Extend the store on the given address with the given value. */
    def extendSto[A <: StoreAddress](adr: A, v: adr.Value): M[Unit]

    /** Update the store on the given address with the given value.
      *
      * @note
      *   used in the context of destructively updating an existing value in the store, whereas `extendSto` is used for introducing new bindings in
      *   the store.
      */
    def updateSto[A <: StoreAddress](adr: A, v: adr.Value): M[Unit]

    // let Scala know to use the instance itself as an implicit
    implicit final private val self: StoreM[M, A, V] = this

/** Monad encoding allocations of new addresses
  *
  * @tparam M
  *   the type of the monadic value that is used for monadic computations
  * @tparam Vec
  *   the type of vectors referenced by addresses to vectors
  * @tparam Pai
  *   the type of pairs referenced by addresses to pairs
  */
trait AllocM[M[_]: Monad, V, Vec, Pai]:
    /** Allocates a new address based on a source code identifier, usually associated with variables
      */
    def allocVar(idn: Identifier[SchemeExp]): M[VarAddress[V]]

    /** Allocates an address for pairs, usually associated with a source location of a pair literal or a primitive that allocates a pair */
    def allocPair(exp: SchemeExp): M[PairAddress[Pai]]

    /** Allocates an address for vectors, usually associated with a source location of a vector literal or a primitive that allocates the vector */
    def allocVec(exp: SchemeExp): M[VectorAddress[Vec]]

/** Monadic operations for implementing Scheme primitives */
trait SchemePrimM[M[_]: Monad, V, Vec, Pai] extends MonadJoin[M] with cats.MonadError[M, Error] with AllocM[M, V, Vec, Pai] with StoreM[M, Address, V]:

    // for convenience
    def storeVec(exp: SchemeExp, vlu: Vec): M[VectorAddress[Vec]] =
        allocVec(exp).flatMap(adr => extendSto(adr, vlu) >> adr.pure)
    def storePair(exp: SchemeExp, vlu: Pai): M[PairAddress[Pai]] =
        allocPair(exp).flatMap(adr => extendSto(adr, vlu) >> adr.pure)

    def deref[X: Lattice, A <: StoreAddress](adr: A)(f: (adr.Value) => M[X]): M[X] =
        lookupSto(adr) >>= f

    def allocList(
        exs: List[SchemeExp],
        vlus: List[V]
      )(using
        dom: SchemeDomain[V, Vec, Pai]
      ): M[V] =
        val lat = dom.schemeLattice
        import lat.given

        exs.zip(vlus).foldM[M, V](Galois.inject[SimpleSchemeValue, V](SchemeNil)) { case (rest, (ex, vlu)) =>
            storePair(ex, dom.pairLattice.cons(vlu, rest)).map(adr => Galois.inject[SimpleSchemeValue, V](SchemePtr(adr)))
        }

    def currentThread: M[TID] = throw new Exception("Not supported")

    // exotic -- not so important if not implemented yet
    // def callcc(clo: (SchemeLambdaExp, Environment[A]), pos: Position): M[V] =
    //  throw new Exception("Not supported")
