package maf.modular.scv

import maf.modular.scheme.modf._
import maf.modular.scheme.SchemeDomain
import maf.language.scheme._
import maf.core._

/**
 * Provides the base Scheme semantics for soft contract verification.
 *
 * The actual contract semantics are implemented in traits extending from this base trait
 */
trait ScvBaseSemantics extends BigStepModFSemanticsT { outer =>
  import SetMonad._
  import MonadStateT._
  import Monad.MonadSyntaxOps

  case class State(env: Environment[Address], store: StoreCache, lstore: BasicStore[Addr, Value])
  object State:
      def empty: State = State(env = BasicEnvironment(Map()), store = Map(), new BasicStore(content = Map()))

  override type EvalM[X] = ScvEvalM[X]
  type ScvEvalM[X] = MonadStateT[State, Set, X]
  type StoreCache = Map[Addr, SchemeExp]

  final lazy val scvMonadInstance: StateOps[State, ScvEvalM] = MonadStateT.stateInstance[State, Set]
  implicit val evalM = new TEvalM:
      import scvMonadInstance.{get, put, withState}
      export scvMonadInstance._
      def getEnv: EvalM[Environment[Address]] = get.map(_.env)
      def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => EvalM[X]): EvalM[X] =
        withState(s => s.copy(env = f(s.env)))(ev)
      def guard(bln: Boolean): EvalM[Unit] = 
        if bln then unit(()) else mzero
      def mzero[X]: EvalM[X] = MonadStateT.lift(Set())
      def merge[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] = 
        for xv <- x ; yv <- y yield Lattice[X].join(xv, yv)
}
