package maf.modular.scheme.modf

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.core.Address
import maf.language.scheme.*
import maf.core.Monad.*
import maf.core.StateOps
import maf.core.monad.ReaderT
import maf.core.Lattice
import maf.core.LocalStore
import maf.language.scheme.SchemeLambdaExp
import maf.core.Environment
import maf.core.Delta
import maf.modular.scheme.modf.SchemeModFComponent.Call
import maf.modular.ModAnalysis
import maf.core.Position.Position
import maf.util.benchmarks.Timeout.T
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.modular.scheme.SchemeSetup
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.core.MonadStateT
import maf.core.Monad
import maf.core
import maf.modular.Dependency

trait TEvalMFlowSensitive[M[_], V: Lattice] extends TEvalM[M]:
    def getStore: M[LocalStore[Address, V]]
    def write(addr: Address, v: V): M[Unit]
    def read(addr: Address): M[V]
    def putStore(lstore: LocalStore[Address, V]): M[Unit]
    def runForValue[T](init: LocalStore[Address, V], initialEnv: Environment[Address], m: M[T]): scala.collection.immutable.Set[T]

    /** Runs the given impure computation inside the monad */
    def impure[X](f: => X): M[X]

trait SchemeModFFlowSensitive extends BigStepModFSemanticsT:
    type Component = FlowSensitiveComponent
    type Store = LocalStore[Address, Value]

    implicit override val evalM: TEvalMFlowSensitive[EvalM, Value]

    //////////////////////////
    // Components
    //////////////////////////

    override def view(cmp: Component): SchemeModFComponent =
        cmp.modfComponent

    /** Flow sensitive components that keep track of a local store in their context. */
    case class FlowSensitiveComponent(modfComponent: SchemeModFComponent, localStore: Store) extends SchemeModFComponent

    /** A map from components to their (cached) results */
    private var componentStores: Map[Component, Store] = Map()

    override def newComponent(call: Call[ComponentContext]): Component =
        throw new Exception("(flow-sensitive) use newComponentM instead of newComponent")

    override protected def newComponentM(call: Call[ComponentContext]): EvalM[Component] =
        evalM.getStore.map(store => FlowSensitiveComponent(call, store))

    // override the "result" method such that the resulting local stores are used
    override def result: Option[Value] =
        val init = initialComponent
        componentStores.get(init).flatMap(_.lookup(returnAddr(init)))

    override def intraAnalysis(component: Component): FlowSensitiveIntra

    // Make sure that the global store operations are not used
    override def writeAddr(addr: Address, value: Value): Boolean =
        throw new Exception("global store writes are not supported (inter)")

    /** The initial store */
    protected def baseStore: LocalStore[Address, Value]

    trait FlowSensitiveIntra extends BigStepModFIntraT:
        override def analyzeWithTimeout(timeout: T): Unit =
            // inject the local store into the analysis context of the component
            val lstores = evalM.runForValue(
              baseStore,
              fnEnv,
              for
                  _ <- evalM.putStore(component.localStore)
                  // the evaluate the expression
                  result <- eval(body(component))
                  // put the result in the store on the correct address
                  _ <- write(returnAddr(component), result)
                  lstore <- evalM.getStore
              yield lstore
            )
            lstores.foreach(lstore =>
                // keep track of the resulting store from the component
                componentStores = componentStores + (component -> lstore)
            )

        // Make sure that the global store operations are not used
        override def writeAddr(addr: Addr, value: Value): Boolean =
            throw new Exception("global store writes are not supported")

        override def readAddr(addr: Addr): Value =
            throw new Exception("global store reads are not supported")

        override def doWrite(dep: Dependency): Boolean = dep match
            case maf.modular.AddrDependency(adr) => true
            case _                               => super.doWrite(dep)

        // Read and writes are
        override protected def read(adr: Addr): EvalM[Value] =
            register(maf.modular.AddrDependency(adr))
            evalM.read(adr)

        override protected def write(adr: Addr, v: Value): EvalM[Unit] =
            given b: (Address => Boolean) = (_) => false
            // Check whether the final component store has a value that subsumes the to-write value
            val prior = componentStores.get(component).getOrElse(LocalStore.empty).lookup(adr).getOrElse(lattice.bottom)
            (if !lattice.subsumes(prior, v) then evalM.impure(trigger(maf.modular.AddrDependency(adr)))
             else evalM.unit(())) >>> evalM.write(adr, v)

        // Override the afterCall function in order to inject the local store into the monad
        override protected def afterCall(vlu: Value, cmp: Component, cll: Position): EvalM[Value] =
            // read the store from the component
            componentStores.get(cmp) match
                case Some(lstore) =>
                    evalM.putStore(lstore).map(_ => vlu)
                case _ =>
                    throw new Exception(s"Unsupported component detected $cmp")

/**
 * Trait that provides a simple implementation of the TEvalMFlowSensitive monad.
 *
 * The monad mimics the behavior of the TEvalM.EvalM instance, but keeps track of a local store in addition to the environment.
 */
trait SimpleFlowSensitiveAnalysisMonad extends SchemeModFFlowSensitive:
    import maf.core.SetMonad.*

    /** The state that is passed through the analysis */
    case class AnalysisState(lstore: Store)

    type Reader = [Y] =>> ReaderT[Set, Environment[Address], Y]

    /**
     * The monad is a combination of a state monad that keeps track of the store, and a reader monad that passes the environment down throughout the
     * evalation.
     */
    type EvalM[X] = MonadStateT[AnalysisState, Reader, X]

    protected val monadInstance: StateOps[AnalysisState, EvalM] = MonadStateT.stateInstance[AnalysisState, Reader]

    given evalM: TEvalMFlowSensitive[EvalM, Value] with
        export monadInstance.*
        import monadInstance.*
        import maf.core.monad.MonadLift.*

        def getStore: EvalM[Store] =
            get.map(_.lstore)

        def putStore(lstore: LocalStore[Address, Value]): EvalM[Unit] =
            get.flatMap(st => put(st.copy(lstore = lstore)))

        def write(addr: Address, v: Value): EvalM[Unit] =
            getStore.flatMap(store => putStore(store.integrate(store.extend(addr, v))))

        def read(addr: Address): EvalM[Value] =
            getStore.flatMap(store =>
                store.lookup(addr) match
                    case Some(vlu) => unit(vlu)
                    case _         => mzero
            )

        def runForValue[V](init: LocalStore[Address, Value], initialEnv: Environment[Address], m: EvalM[V]): scala.collection.immutable.Set[V] =
            m.runValue(AnalysisState(init)).runReader(initialEnv)

        def fail[X](err: core.Error): EvalM[X] =
            /* ignore error */
            mzero

        def merge[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] = /* do not merge branches, but continue non-deterministically */
            MonadStateT((s) => ReaderT((e) => x.run(s).runReader(e) ++ y.run(s).runReader(e)))

        def mzero[X]: EvalM[X] =
            lift(ReaderT.lift(Set()))

        def getEnv: EvalM[Environment[Address]] =
            lift(ReaderT.ask)

        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => EvalM[X]): EvalM[X] =
            MonadStateT((s) => ReaderT.local(f)(ev.run(s)))

/* Convience implementation of a flow sensitive analysis */
class SimpleFlowSensitiveAnalysis(exp: SchemeExp)
    extends ModAnalysis[SchemeExp](exp)
    with SchemeModFFlowSensitive
    with SchemeModFNoSensitivity
    with StandardSchemeModFAllocator
    with SimpleFlowSensitiveAnalysisMonad
    with SchemeSetup
    with SchemeConstantPropagationDomain
    with FIFOWorklistAlgorithm[SchemeExp]:

    override def baseEnv: Env = initialEnv
    override lazy val baseStore: LocalStore[Address, Value] =
        given lat: Lattice[Value] = this.lattice
        given shouldCount: (Addr => Boolean) = ((_) => false)
        LocalStore.from[Address, Value](store)

    override def initialComponent: Component =
        FlowSensitiveComponent(SchemeModFComponent.Main, baseStore)

    override def intraAnalysis(component: Component): FlowSensitiveIntra =
        new IntraAnalysis(component) with FlowSensitiveIntra
