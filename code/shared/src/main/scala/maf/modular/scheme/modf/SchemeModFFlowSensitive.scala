package maf.modular.scheme.modf

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.core.Address
import maf.core.Lattice
import maf.core.LocalStore
import maf.language.scheme.SchemeLambdaExp
import maf.core.Environment
import maf.core.Delta

trait TEvalMFlowSensitive[M[_], V: Lattice] extends TEvalM[M]

trait SchemeModFFlowSensitive extends BigStepModFSemanticsT:
    type Component = FlowSensitiveComponent

    /** Flow sensitive components that keep track of a local store in their context. */
    sealed trait FlowSensitiveComponent extends Serializable
    case class Main(store: LocalStore[Address, Value])
    case class Call[Context](clo: (SchemeLambdaExp, Environment[Address]), ctx: Context)

    /** A map from components to their (cached) results */
    private var componentResults: Map[Component, Value] = Map()

    /** A map from components to their manipulations on the store */
    private var deltaStores: Map[Component, Delta[Address, Value]] = Map()

    type EvalM[_] <: TEvalMFlowSensitive[_, Value]
