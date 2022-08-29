package maf.language.AScheme.lattices

import maf.core.*
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.interfaces.*
import maf.language.AScheme.ASchemeValues.*
import maf.lattice.HMap
import maf.lattice.AbstractSetType
import maf.language.AScheme.ASchemeLattice

class ASchemeModularLattice[A <: Address, S: StringLattice, B: BoolLattice, I: IntLattice, R: RealLattice, C: CharLattice, Sym: SymbolLattice]
    extends ModularSchemeLattice[A, S, B, I, R, C, Sym],
      ASchemeLattice[HMap, A]:

    object ActorT extends AbstractSetType[Actor, Actors]:
        def wrap = Actors.apply

    object BehaviorT extends AbstractSetType[Behavior, Behaviors]:
        def wrap = Behaviors.apply

    object FutureT extends AbstractSetType[Future, Futures]:
        def wrap = Futures.apply

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

    def getActors(x: L): Set[Actor] = x.get(ActorT)
    def getBehs(x: L): Set[Behavior] = x.get(BehaviorT)
    def getFutures(x: L): Set[Future] = x.get(FutureT)

    def actor(actor: Actor): L = HMap.injected(ActorT, actor)
    def beh(behavior: Behavior): L = HMap.injected(BehaviorT, behavior)
    def future(fut: Future): L = HMap.injected(FutureT, fut)
