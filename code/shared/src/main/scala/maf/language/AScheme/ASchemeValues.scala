package maf.language.AScheme

import maf.lattice.interfaces.*
import maf.util.SmartHash
import maf.lattice.interfaces.BoolLattice
import maf.language.scheme.ASchemeActor
import maf.language.scheme.*
import maf.core.{Address, Environment, Identifier}
import maf.util.Show
import maf.core.Lattice
import maf.core.LatticeTopUndefined
import maf.util.datastructures.MapOps.*
import maf.language.scheme.lattices.SchemeLattice

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
                case ASchemeSelect(handlers, idn) =>
                    handlers
                        .filter { case (key, value) =>
                            val keySymbol = lat.symbol(key)
                            lat.mayEql(keySymbol, tag)
                        }
                        .map { case (tag, (ags, bdy)) => SchemeLambda(Some(tag), ags, bdy, None, idn) }
                        .toSet

                case _ => throw new Exception("Invalid body of the behavior")

    def EmptyBehavior(bdy: SchemeExp): Behavior = Behavior(Some("<empty>"), List(), bdy, Environment.empty, false)

    /** The class of futures supported by AScheme */
    sealed trait Future extends ASchemeValue

    /**
     * A future waiting for an actor to complete
     *
     * @param tid
     *   the id of the actor we are waiting for
     */
    case class ActorWaitCompleteFuture(tid: AID) extends Future

    /** A representation for messages */
    case class Message[Value](tag: String, vlus: List[Value], exs: List[SchemeExp] = List()) extends ASchemeValue:
        def mapValues[Y](f: Value => Y): Message[Y] =
            this.copy(vlus = vlus.map(f))

    object Message:
        given showMessage[Value]: Show[Message[Value]] with
            def show(m: Message[Value]) = m.toString

        /**
         * A lattice implementation for a set of messages (i.e. a bounded powerset based mailbox).
         *
         * The join of these mailboxes proceeds as follows:
         *   - each message with the same tag gets joined together
         *   - a message is joined by joining the values of the arguments together
         *
         * Therefore, in the powerset mailbox abstraction, order and multiplicity of messages is not preserved.
         */
        given messageSetLattice[Value: Lattice]: Lattice[Set[Message[Value]]] with
            type L = Set[Message[Value]]
            def bottom: L = Set()
            def top: L = throw LatticeTopUndefined
            def join(x: L, y: => L): L =
                (x ++ y)
                    // collect all messages with the same tag and arity in `x`
                    .groupBy(msg => (msg.tag, msg.vlus.size))
                    // join all the matching arguments together
                    .map { case ((tag, arity), msgs) =>
                        val ags = msgs.map(_.vlus).reduce((lastVlus, vlus) => lastVlus.zip(vlus).map { case (a, b) => Lattice[Value].join(a, b) })
                        Message(tag, ags)

                    }
                    .toSet

            def subsumes(x: L, y: => L): Boolean =
                y.subsetOf(x)
            def eql[B: BoolLattice](x: L, y: L): B = ??? // TODO: implement
            def show(x: L): String = x.toString
