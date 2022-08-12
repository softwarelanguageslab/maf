package maf.modular.scheme.modf

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.core.Address
import maf.core.Monad.*
import maf.core.Lattice
import maf.core.LocalStore
import maf.language.scheme.SchemeLambdaExp
import maf.core.Environment
import maf.core.Delta
import maf.modular.scheme.modf.SchemeModFComponent.Call
import maf.modular.ModAnalysis
import maf.core.Position.Position
import maf.util.benchmarks.Timeout.T

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

    /** Flow sensitive components that keep track of a local store in their context. */
    case class FlowSensitiveComponent(modfComponent: SchemeModFComponent, localStore: Store) extends SchemeModFComponent

    /** A map from components to their (cached) results */
    private var componentResults: Map[Component, Value] = Map()

    /** A map from components to their manipulations on the store */
    private var resultStores: Map[Component, Store] = Map()

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
            evalM.runForValue(for
                _ <- initialState.flatMap(_ => evalM.putStore(component.localStore))
                // the evaluate the expression
                result <- eval(body(component))
                // put the result in the store on the correct address
                _ <- write(returnAddr(component), result)
                lstore <- evalM.getStore
            yield lstore)

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
            resultStores.get(cmp) match
                case Some(lstore) =>
                    evalM.putStore(lstore).map(_ => vlu)
                case _ =>
                    throw new Exception(s"Unsupported component detected $cmp")
