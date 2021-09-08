package maf.modular.scv

import maf.modular.ModAnalysis
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.language.scheme._
import maf.modular.scheme.{SchemeDomain}
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.core.{Environment, Lattice, Monad, MonadStateT, SetMonad, Store, BasicStore}
import maf.util.benchmarks.Timeout

/** This trait encodes the semantics of the ContractScheme language */
trait ScvSemantics extends ModAnalysis[SchemeExp] with ScvModAnalysis with SchemeModFLocalSensitivity with SchemeSemantics { outer =>
  import SetMonad._
  import MonadStateT._
  import Monad.MonadSyntaxOps

  case class State(env: Environment[Adr], store: Store[Adr, Val], cache: StoreCache)
  object State:
      def empty: State = State(Environment.empty, new BasicStore(content = Map()), Map())

  type A[X] = ScvEvalM[X]
  type ScvEvalM[X] = MonadStateT[State, Set, X]
  type ScMonadInstance = StateInstance[State, Set]
  type StoreCache = Map[Adr, SchemeExp]

  final lazy val scvMonadInstance: ScMonadInstance  = MonadStateT.stateInstance[State, Set]

  given analysisM: AnalysisM[ScvEvalM] with
      import scvMonadInstance.{get, put}
      private type M[X] = ScvEvalM[X]

      // MONAD
      export scvMonadInstance._

      // MONADJOIN
      def mbottom[X]: M[X] = MonadStateT.lift(Set.empty)
      def mjoin[X: Lattice](x: M[X], y: M[X]): M[X] =
        for
            xv <- x
            yv <- y
        yield Lattice[X].join(xv, yv)

      // MONADERROR
      def fail[X](err: maf.core.Error) =
        mbottom // TODO: should probably store this error somewhere

      // STOREM
      def addrEq = ??? // TODO
      def extendSto(a: Addr, v: Val): M[Unit] = 
        withState(state => state.copy(store = state.store.extend(a, v))) { unit(()) }

      def lookupSto(a: Addr): M[Val] = 
        get.map(_.store.lookup(a)).flatMap(_.map(unit).getOrElse(lift(SetMonad.fail)))
        
      def updateSto(a: Addr, v: Val): M[Unit] = 
        withState(state => state.copy(store = state.store.update(a, v))) { unit(()) }

      // ANALYSISM
      def call(lam: Lam): M[Val] = ???

      def getCtx: M[Ctx] = 
        throw new Exception("local contexts not supported")

      def getEnv: ScvEvalM[Env] =
        get.map(_.env)

      def withCtx[X](ctx: Ctx => Ctx)(blk: M[X]): M[X] = 
        throw new Exception("local contexts not supported")

      def withEnv[X](f: Env => Env)(blk: M[X]): M[X] =
        withState(state => state.copy(env = f(state.env)))(blk)

  override def intraAnalysis(component: Component): IntraScvSemantics

  trait IntraScvSemantics extends IntraScvAnalysis:
      override def analyzeWithTimeout(timeout: Timeout.T): Unit =
        outer.eval(program).run(State.empty)
}
