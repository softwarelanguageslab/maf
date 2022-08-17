package maf.modular.scheme.modf

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.core.Address
import maf.language.scheme.*
import maf.core.Monad.*
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
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.core.MonadStateT
import maf.core.Monad
import maf.core

trait TEvalMFlowSensitive[M[_], V: Lattice] extends TEvalM[M]:
    def getStore: M[LocalStore[Address, V]]
    def write(addr: Address, v: V): M[Unit]
    def read(addr: Address): M[V]
    def putStore(lstore: LocalStore[Address, V]): M[Unit]
    def runForValue[V](m: M[V]): V

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

    override def intraAnalysis(component: Component): FlowSensitiveIntra

    // Make sure that the global store operations are not used
    override def writeAddr(addr: Address, value: Value): Boolean =
        throw new Exception("global store writes are not supported (inter)")

    trait FlowSensitiveIntra extends BigStepModFIntraT:
        /** Returns an EvalM monad that has initialized its state to the initial state */
        protected def initialState: EvalM[Unit]

        override def analyzeWithTimeout(timeout: T): Unit =
            // inject the local store into the analysis context of the component
            val lstore = evalM.runForValue(for
                _ <- initialState.flatMap(_ => evalM.putStore(component.localStore))
                // the evaluate the expression
                result <- eval(body(component))
                // put the result in the store on the correct address
                _ <- write(returnAddr(component), result)
                lstore <- evalM.getStore
            yield lstore)
            // keep track of the resulting store from the component
            componentStores = componentStores + (component -> lstore)

        // Make sure that the global store operations are not used
        override def writeAddr(addr: Addr, value: Value): Boolean =
            throw new Exception("global store writes are not supported")

        override def readAddr(addr: Addr): Value =
            throw new Exception("global store reads are not supported")

        // Read and writes are
        override protected def read(adr: Addr): EvalM[Value] =
            register(maf.modular.AddrDependency(adr))
            evalM.read(adr)

        override protected def write(adr: Addr, v: Value): EvalM[Unit] =
            trigger(maf.modular.AddrDependency(adr))
            evalM.write(adr, v)

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

    protected val monadInstance: Monad[EvalM] = MonadStateT.stateInstance[AnalysisState, Reader]

    given evalM: TEvalMFlowSensitive[EvalM, Value] with
        export monadInstance.*
        def getStore: EvalM[Store] = ???
        def putStore(lstore: LocalStore[Address, Value]): EvalM[Unit] = ???
        def write(addr: Address, v: Value): EvalM[Unit] = ???
        def read(addr: Address): EvalM[Value] = ???
        def runForValue[V](m: EvalM[V]): V = ???
        def fail[X](err: core.Error): EvalM[X] = ???
        def merge[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] = ???
        def mzero[X]: EvalM[X] = ???
        def getEnv: EvalM[Environment[Address]] = ???
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => EvalM[X]): EvalM[X] = ???

/* Convience implementation of a flow sensitive analysis */
class SimpleFlowSensitiveAnalysis(exp: SchemeExp)
    extends ModAnalysis[SchemeExp](exp)
    with SchemeModFFlowSensitive
    with SchemeModFNoSensitivity
    with StandardSchemeModFAllocator
    with FIFOWorklistAlgorithm[SchemeExp]
    with SchemeConstantPropagationDomain
    with SimpleFlowSensitiveAnalysisMonad:

    override def store: Map[Address, Value] = ???
    override def store_=(store: Map[Address, Value]): Unit = ???

    override def baseEnv: Env = ???
    override def initialComponent: Component = ???

    override def intraAnalysis(component: Component): FlowSensitiveIntra =
        new IntraAnalysis(component) with FlowSensitiveIntra:
            override protected def initialState: EvalM[Unit] = ???
