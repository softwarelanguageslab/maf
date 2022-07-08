package maf.language.AScheme

import maf.util.SmartHash
import maf.lattice.interfaces.BoolLattice
import maf.language.scheme.ASchemeActor
import maf.language.scheme.*
import maf.core.{Address, Environment, Identifier}
import maf.util.Show
import maf.core.Lattice
import maf.core.LatticeTopUndefined
import maf.util.datastructures.MapOps.*

object ASchemeValues:
    sealed trait ASchemeValue

    /** An actor identifier */
    trait AID extends SmartHash with ASchemeValue:
        def removeEnv: AID

    /** A simple actor identifier based on a sequential integer */
    case class SimpleActorId(id: Int) extends AID:
        def removeEnv: SimpleActorId = this

    /**
     * A running actor.
     *
     * @param name
     *   an optional of the actor
     * @param TID
     *   The associated actor id (usually represented by a component)
     */
    case class Actor(name: Option[String], tid: AID) extends ASchemeValue:
        override def toString: String = s"$name"
        def removeEnv: Actor = this.copy(tid = tid.removeEnv)

    /** Represents a behavior */
    case class Behavior(name: Option[String], prs: List[Identifier], bdy: SchemeExp, lexEnv: Environment[Address]) extends ASchemeValue:
        def removeEnv: Behavior = this.copy(lexEnv = Environment.empty)
        override def toString: String = s"<behavior: $name>"

    def EmptyBehavior(bdy: SchemeExp): Behavior = Behavior(Some("<empty>"), List(), bdy, Environment.empty)

    case class Message[Value](tag: String, vlus: List[Value]) extends ASchemeValue:
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
