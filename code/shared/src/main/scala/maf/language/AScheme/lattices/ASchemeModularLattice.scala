package maf.language.AScheme.lattices

import maf.core.*
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.interfaces.*
import maf.language.AScheme.ASchemeValues.*
import maf.lattice.HMap
import maf.lattice.AbstractSetType
import maf.language.AScheme.ASchemeLattice
import maf.modular.scheme.modactor.MirrorValues.Mirror

class ASchemeModularLattice[A <: Address, S: StringLattice, B: BoolLattice, I: IntLattice, R: RealLattice, C: CharLattice, Sym: SymbolLattice]
    extends ModularSchemeLattice[A, S, B, I, R, C, Sym],
      ASchemeLattice[HMap, A]:

    object ActorT extends AbstractSetType[Actor, Actors]:
        def wrap = Actors.apply
        lazy val oldLattice: Lattice[Set[Actor]] = summon[Lattice[Set[Actor]]]
        override val lattice: Lattice[Set[Actor]] = new Lattice[Set[Actor]]:
            export oldLattice.{subsumes => _, *}
            def subsumes(x: Set[Actor], y: => Set[Actor]): Boolean =
                val xs = x.map(a => a.copy(tid = a.tid.removeEnv.removeContext))
                val ys = y.map(b => b.copy(tid = b.tid.removeEnv.removeContext))
                oldLattice.subsumes(xs, ys)

    object BehaviorT extends AbstractSetType[Behavior, Behaviors]:
        def wrap = Behaviors.apply

    object FutureT extends AbstractSetType[Future, Futures]:
        def wrap = Futures.apply

    object MessageT extends AbstractSetType[ReifiedMessage, Messages]:
        def wrap = Messages.apply

    object MirrorT extends AbstractSetType[Mirror[Actor], Mirrors]:
        def wrap = Mirrors.apply
    case class Actors(actors: Set[Actor]) extends Value, Product1[Set[Actor]]:
        def ord = 27
        def typeName = "ACTOR"
        val tpy = ActorT
        override def toString: String = s"<actor {${actors.mkString(",")}}>"

    case class Behaviors(behs: Set[Behavior]) extends Value, Product1[Set[Behavior]]:
        def ord = 28
        def typeName = "BEH"
        val tpy = BehaviorT
        override def toString: String = s"<behavior>"
    case class Futures(futures: Set[Future]) extends Value, Product1[Set[Future]]:
        def ord = 29
        def typeName = "FUT"
        val tpy = FutureT
        override def toString: String = s"<future>"

    case class Messages(messages: Set[ReifiedMessage]) extends Value, Product1[Set[ReifiedMessage]]:
        def ord = 30
        def typeName = "MSG"
        val tpy = MessageT
        override def toString: String = s"<message: {${messages.map(_.toString).mkString(",")}}>"

    case class Mirrors(mirror: Set[Mirror[Actor]]) extends Value, Product1[Set[Mirror[Actor]]]:
        def ord = 31
        def typeName = "MIR"
        val tpy = MirrorT
        override def toString: String = s"<mirror>"

    def getActors(x: L): Set[Actor] = x.get(ActorT)
    def getBehs(x: L): Set[Behavior] = x.get(BehaviorT)
    def getFutures(x: L): Set[Future] = x.get(FutureT)
    def getMessages(x: L): Set[ReifiedMessage] = x.get(MessageT)
    def getMirrors(x: L): Set[Mirror[Actor]] = x.get(MirrorT)

    def actor(actor: Actor): L = HMap.injected(ActorT, actor)
    def beh(behavior: Behavior): L = HMap.injected(BehaviorT, behavior)
    def future(fut: Future): L = HMap.injected(FutureT, fut)
    def mirrors(x: Mirror[Actor]): L = HMap.injected(MirrorT, x)
    def message(m: ReifiedMessage): L = HMap.injected(MessageT, m)
