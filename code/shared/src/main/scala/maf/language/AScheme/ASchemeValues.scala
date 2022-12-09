package maf.language.AScheme

import maf.lattice.interfaces.*
import maf.util.SmartHash
import maf.core.Monad.*
import maf.core.Monad
import maf.lattice.interfaces.BoolLattice
import maf.language.scheme.ASchemeActor
import maf.language.scheme.*
import maf.core.{Address, Environment, Identifier}
import maf.util.Show
import maf.core.Lattice
import maf.core.LatticeTopUndefined
import maf.util.datastructures.MapOps.*
import maf.language.scheme.lattices.SchemeLattice
import maf.language.scheme.primitives.SchemeLatticePrimitives
import maf.language.scheme.primitives.SchemePrimM
import maf.core.Identity
import maf.language.scheme.primitives.SchemePrimitives

object ASchemeValues:
    sealed trait ASchemeValue

    /** An actor identifier */
    trait AID extends SmartHash with ASchemeValue:
        def removeEnv: AID
        def removeContext: AID

    /** A simple actor identifier based on a sequential integer */
    case class SimpleActorId(id: Int) extends AID:
        def removeEnv: SimpleActorId = this
        def removeContext: SimpleActorId = this

    /**
     * A running actor.
     *
     * @param name
     *   an optional of the actor
     * @param TID
     *   The associated actor id (usually represented by a component)
     */
    case class Actor(name: Option[String], tid: AID) extends ASchemeValue:
        override def toString: String = s"$name, $tid"
        def removeEnv: Actor = this.copy(tid = tid.removeEnv)

    /** Represents a behavior */
    case class Behavior(name: Option[String], prs: List[Identifier], bdy: SchemeExp, lexEnv: Environment[Address], isMirror: Boolean)
        extends ASchemeValue:
        def removeEnv: Behavior = this.copy(lexEnv = Environment.empty)
        override def toString: String = s"<behavior: $name>"

        /** Returns the handler associated with the given tag */
        def lookupHandler(tag: String): Option[SchemeExp] =
            bdy match
                case ASchemeSelect(handlers, idn) =>
                    handlers.get(tag).map { case (ags, bdy) =>
                        SchemeLambda(Some(tag), ags, bdy, None, idn)
                    }

                case _ => throw new Exception("Invalid body of the behavior")

        /**
         * Returns the handler associated with the given symbol
         *
         * @note
         *   the performance of this is O(n) where n is the number of handlers.
         */
        def lookupHandler[L](tag: L)(using lat: SchemeLattice[L, maf.core.Address]): Set[SchemeExp] =
            bdy match
                case select: ASchemeSelect => select.lookupHandler(tag, wrap = true).map(_._2)
                case _                     => throw new Exception("Invalid behavior body")

    def EmptyBehavior(bdy: SchemeExp): Behavior = Behavior(Some("<empty>"), List(), bdy, Environment.empty, false)

    extension (select: ASchemeSelect)
        /**
         * Returns the handler associated with the given symbol
         *
         * @note
         *   the performance of this is O(n) where n is the number of handlers.
         */
        def lookupHandler[L](tag: L, wrap: Boolean = false)(using lat: SchemeLattice[L, maf.core.Address]): Set[(List[Identifier], SchemeExp)] =
            select.handlers
                .filter { case (key, value) =>
                    val keySymbol = lat.symbol(key)
                    lat.mayEql(keySymbol, tag)
                }
                .map { case (tag, (ags, bdy)) =>
                    (ags, if wrap then SchemeLambda(Some(tag), ags, bdy, None, select.idn) else SchemeBegin(bdy, select.idn))
                }
                .toSet

    /** The class of futures supported by AScheme */
    sealed trait Future extends ASchemeValue

    /**
     * A future waiting for an actor to complete
     *
     * @param tid
     *   the id of the actor we are waiting for
     */
    case class ActorWaitCompleteFuture(tid: AID) extends Future

    sealed trait AbstractMessage[L]:
        def tag[M[_]](using SchemeLattice[L, Address], SchemePrimM[M, Address, L]): M[L]
        def vlus[M[_]](using SchemeLattice[L, Address], SchemePrimM[M, Address, L]): M[L]

        /** Prepend a value to the payload */
        def prepend[M[_]](vlu: L)(using SchemeLattice[L, Address], SchemePrimM[M, Address, L], SchemePrimitives[L, Address]): M[AbstractMessage[L]]
        def toMetaMessage[M[_]](using SchemeLattice[L, Address], SchemePrimM[M, Address, L]): M[MetaMessage[L]] =
            (tag, vlus) mapN ((tag, vlus) => MetaMessage(tag, vlus))

    /** A representation for messages */
    case class Message[Value](_tag: String, _vlus: List[Value], _exs: List[SchemeExp] = List()) extends ASchemeValue, AbstractMessage[Value]:
        def prepend[M[_]](
            vlu: Value
          )(using SchemeLattice[Value, Address],
            SchemePrimM[M, Address, Value],
            SchemePrimitives[Value, Address]
          ): M[AbstractMessage[Value]] =
            Monad[M].unit(this.copy(_vlus = vlu :: _vlus))
        def mapValues[Y](f: Value => Y): Message[Y] =
            this.copy(_vlus = _vlus.map(f))

        def tag[M[_]](using lat: SchemeLattice[Value, Address], primM: SchemePrimM[M, Address, Value]): M[Value] =
            primM.unit(lat.symbol(_tag))

        def vlus[M[_]](using lat: SchemeLattice[Value, Address], primM: SchemePrimM[M, Address, Value]): M[Value] =
            primM.allocList(_exs, _vlus)

    case class MessagePayloadAddress(base: Address, idx: Int) extends Address:
        override def idn: Identity = base.idn
        override def printable: Boolean = true

    private def allocPayloadAddr[M[_], L](base: Identity, idx: Int)(using primM: SchemePrimM[M, Address, L]): M[Address] =
        primM.allocPtr(ASchemeSynthetic(base)).map(MessagePayloadAddress(_, idx))

    private def allocLstBase[M[_], L](
        lst: List[L],
        base: Identity,
        ctr: Int
      )(using primM: SchemePrimM[M, Address, L],
        lat: SchemeLattice[L, Address]
      ): M[L] = lst match
        case Nil => primM.unit(lat.nil)
        case x :: xs =>
            for
                rest <- allocLstBase(xs, base, ctr + 1)
                addr <- allocPayloadAddr(base, ctr)
                _ <- primM.extendSto(addr, lat.cons(x, rest))
            yield lat.pointer(addr)
    object Message:

        /** Create a Scheme list representation of the given values based on a single identity */
        def meta[M[_], L](
            _tag: String,
            _vlus: List[L],
            base: Identity
          )(using primM: SchemePrimM[M, Address, L],
            lat: SchemeLattice[L, Address]
          ): M[MetaMessage[L]] =
            allocLstBase(_vlus, base, 1).map { payload =>
                MetaMessage(lat.symbol(_tag), payload, base = Some(base), siz = Some(_vlus.size))
            }

        def meta[M[_], L](
            _tag: L,
            _vlus: List[L],
            base: Identity
          )(using primM: SchemePrimM[M, Address, L],
            lat: SchemeLattice[L, Address]
          ): M[MetaMessage[L]] =
            allocLstBase(_vlus, base, 1).map { payload =>
                MetaMessage(_tag, payload, base = Some(base), siz = Some(_vlus.size))
            }

        given showMessage[Value]: Show[Message[Value]] with
            def show(m: Message[Value]) = m.toString

    /**
     * A message produced by the meta-layer. It contains abstract values which will be used at the base level to deliver the message in the correct
     * mailbox.
     *
     * @param tag
     *   the tag of the message as an abstract value
     * @param vlus
     *   the possible values in the payload of the message, should be a Scheme list.
     */
    case class MetaMessage[L](_tag: L, _vlus: L, base: Option[Identity] = None, siz: Option[Int] = None) extends ASchemeValue, AbstractMessage[L]:

        def tag[M[_]](using SchemeLattice[L, Address], SchemePrimM[M, Address, L]): M[L] = Monad[M].unit(_tag)
        def vlus[M[_]](using SchemeLattice[L, Address], SchemePrimM[M, Address, L]): M[L] = Monad[M].unit(_vlus)
        override def toMetaMessage[M[_]](using SchemeLattice[L, Address], SchemePrimM[M, Address, L]): M[MetaMessage[L]] = Monad[M].unit(this)

        def prepend[M[_]](
            vlu: L
          )(using lat: SchemeLattice[L, Address],
            primM: SchemePrimM[M, Address, L],
            prims: SchemePrimitives[L, Address]
          ): M[AbstractMessage[L]] =
            if base.isDefined then
                allocPayloadAddr(base.get, siz.get + 1).flatMap { adr =>
                    primM.extendSto(adr, lat.cons(vlu, _vlus)).map(_ => this.copy(siz = siz.map(_ + 1), _vlus = lat.pointer(adr)))
                }
            else (vlu ::: _vlus) map (vlus => this.copy(_vlus = vlus))

    extension [L](v: L)(using SchemeLattice[L, Address])
        def car[M[_]](primitives: SchemePrimitives[L, Address])(using SchemePrimM[M, Address, L]): M[L] =
            primitives("car").call(SchemeVar(Identifier("car", Identity.none)), List(v))

        def cdr[M[_]](primitives: SchemePrimitives[L, Address])(using SchemePrimM[M, Address, L]): M[L] =
            primitives("cdr").call(SchemeVar(Identifier("cdr", Identity.none)), List(v))

        def :::[M[_]](car: L)(using prims: SchemePrimM[M, Address, L], primitives: SchemePrimitives[L, Address]): M[L] =
            primitives("cons").call(SchemeVar(Identifier("cons", Identity.none)), List(car, v))

        /** Take n elements from the payload */
        def take[M[_]](primitives: SchemePrimitives[L, Address])(n: Int)(using m: SchemePrimM[M, Address, L]): M[List[L]] =
            if n == 0 then m.unit(List())
            else
                for
                    head <- v.car(primitives)
                    tail <- v.cdr(primitives)
                    result <- tail.take(primitives)(n - 1).map(head :: _)
                yield result
