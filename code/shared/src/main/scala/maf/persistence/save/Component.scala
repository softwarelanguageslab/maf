package maf.save

import io.bullet.borer.Encoder
import io.bullet.borer.Encoder.forIterableOnce
import io.bullet.borer.Writer
import maf.core.Address
import maf.core.Expression
import maf.core.Position.Position
import maf.language.scheme.SchemeExp
import maf.modular.AddrDependency
import maf.modular.AnalysisResults
import maf.modular.Dependency
import maf.modular.scheme.modf.SchemeModFComponent
import maf.modular.scheme.modf.StandardSchemeModFComponents
import io.bullet.borer.derivation.MapBasedCodecs
import maf.language.scheme.SchemeLambdaExp
import maf.core.Identifier
import maf.core.Identity
import maf.core.IdentityData
import maf.language.scheme.SchemeFuncall
import maf.language.scheme.SchemeLambda
import io.bullet.borer.derivation.ArrayBasedCodecs
import maf.language.scheme.SchemeVarArgLambda
import maf.language.scheme.SchemeIf
import maf.language.scheme.SchemeLet
import maf.language.scheme.SchemeVar
import maf.core.BasicEnvironment
import maf.core.Environment
import maf.core.WrappedEnv
import maf.core.NestedEnv
import maf.modular.scv.ScvContextSensitivity
import maf.modular.scheme.modf.NoContext
import maf.core.Position
import maf.core.Position.PTag
import scala.collection.mutable.HashMap
import maf.language.scheme.SchemeLetrec
import maf.language.scheme.SchemeAssert
import maf.language.scheme.SchemeValue
import maf.language.scheme.SchemeSet
import maf.language.scheme.SchemeBegin
import maf.language.scheme.SchemeLetStar
import maf.save.save.SaveExpressions
import maf.core.worklist.WorkList
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.modular.ModAnalysis
import io.bullet.borer.derivation.CompactMapBasedCodecs

/**
 * Trait to encode positions.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SavePosition[Expr <: Expression] extends Save[Expr]:
    given Encoder[Position] = CompactMapBasedCodecs.deriveAllEncoders[Position]
    given Encoder[PTag] = CompactMapBasedCodecs.deriveAllEncoders[PTag]
    given Encoder[Identifier] = CompactMapBasedCodecs.deriveEncoder[Identifier]
    given Encoder[Identity] = CompactMapBasedCodecs.deriveAllEncoders[Identity]
    given Encoder[IdentityData] with
        def write(writer: Writer, value: IdentityData): Writer =
            System.err.nn.println("IdentityData could not be encoded")
            writer

/**
 * The base trait for encoding components.
 *
 * @note
 *   This trait gives the methods needed to encode components, but does not implement them yet, other traits like [[SaveStandardSchemeComponents]] or
 *   [[SaveStandardSchemeComponentID]] should be mixed in for the implementation. The trait that should be mixed in depends on the kind of components
 *   are used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveComponents[Expr <: Expression] extends ModAnalysis[Expr] with Save[Expr]:
    /** Encodes a component. */
    given componentEncoder: Encoder[Component]

protected trait SaveActualComps[Expr <: Expression] extends SaveComponents[Expr]:
    given actualComponentEncoder: Encoder[Component]

trait SaveActualComponents[Expr <: Expression] extends SaveActualComps[Expr]:
    override given componentEncoder: Encoder[Component] = actualComponentEncoder

/**
 * Base trait for encoding components only by their ID, and not in their entirety.
 *
 * @note
 *   This trait gives the methods needed to encode components using their ID, instead of saving it entirely, but no implementation. Other traits like
 *   [[SaveStandardSchemeComponentID]] should be mixed in to give the implementation based on what ID you want to use and what components your
 *   analysis uses.
 *
 * @note
 *   Because this trait only encodes the component IDs, the entire component should be encoded somewhere else if you want to decode this again.
 *
 * @tparam T
 *   The type of the value the needs to be saved
 */
trait SaveComponentID[Expr <: Expression] extends SaveComponents[Expr] with SavePosition[Expr]:
    /** Encodes a component by their ID */
    given componentIDEncoder: Encoder[Component]
    override given componentEncoder: Encoder[Component] = componentIDEncoder

    /** Encodes a set of components */
    protected given componentSetEncoder: Encoder[Set[Component]]
    override def saveInfo: List[(String, Savable[_])] = super.saveInfo ++ List(("components", Savable(visited)))

/**
 * Trait that encodes components using an autoincreasing integer ID.
 *
 * Implementation of [[SaveComponentID]]
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveComponentIntID[Expr <: Expression] extends SaveActualComps[Expr] with SaveComponentID[Expr]:
    private val components = HashMap[Component, Int]()
    private var id = 0

    override protected given componentSetEncoder: MapEncoder[Set[Component]] with
        override def write(writer: Writer, components: Set[Component]): Writer =
            writer.start()
            for (component <- components) do
                SaveComponentIntID.this.components.addOne((component, id))
                writer.writeMember(id.toString(), component)(using actualComponentEncoder)
                id += 1
            writer.close()

    override given componentIDEncoder: Encoder[Component] with
        override def write(writer: Writer, component: Component): Writer = writer.write(components(component))

    override def startSave(): Unit =
        id = 0
        components = HashMap[Component, Int]()
        super.startSave()

/**
 * Trait that encodes components using their position.
 *
 * Implementation of [[SaveComponentID]]
 *
 * @note
 *   Because this trait only encodes the component position, the entire component should be encoded somewhere else if you want to decode this again.
 */
trait SaveStandardSchemeComponentPosition extends SaveComponentID[SchemeExp] with StandardSchemeModFComponents:
    /** Encodes a component by their position */
    override given componentIDEncoder: Encoder[Component] with
        def write(writer: Writer, component: Component): Writer =
            writer.start()
            if component.equals(initialComponent) then writer.write("main")
            else writer.write(component.asInstanceOf[SchemeModFComponent.Call[ComponentContext]])(schemeComponentIDEncoder)
            writer.close()

    /** Encodes a scheme component using their position */
    given schemeComponentIDEncoder[T]: Encoder[SchemeModFComponent.Call[T]] with
        def write(writer: Writer, component: SchemeModFComponent.Call[T]): Writer =
            writer.start()
            val (lambda, _) = component.clo
            writer.write(lambda.idn.pos)
            writer.close()

    override protected given componentSetEncoder: ArrayEncoder[Set[Component]] with
        override def write(writer: Writer, components: Set[Component]): Writer =
            writer.start()
            for (component <- components) do writer.writeMember(component)
            writer.close()

/**
 * Trait to encode environments.
 *
 * @tparam T
 *   The type of the value the needs to be saved
 */
trait SaveEnvironment[Expr <: Expression] extends Save[Expr] with SaveAddr[Expr]:
    given Encoder[BasicEnvironment[Address]] with
        override def write(writer: Writer, env: BasicEnvironment[Address]): Writer = writer.write(env.content)

    given MapEncoder[NestedEnv[Address, Address]] with
        override def write(writer: Writer, env: NestedEnv[Address, Address]): Writer =
            writer.start()
            writer.writeMember("content", env.content)
            writer.writeMember("rst", env.rst)
            writer.close()

    given MapEncoder[Environment[Address]] with
        override def write(writer: Writer, env: Environment[Address]): Writer =
            writer.start()
            env match {
                case basicEnv @ BasicEnvironment(_) =>
                    writer.writeMember("basicEnvironment", basicEnv)
                case nestedEnv @ NestedEnv(_, _) =>
                    writer.writeMember("nestedEnvironment", nestedEnv.asInstanceOf[NestedEnv[Address, Address]])
                case _ =>
                    System.err.nn.println("The environemnt with type `" + env.getClass + "` could not be encoded")
                    writer
            }
            writer.close()

/**
 * Base trait for saving context.
 *
 * @note
 *   This trait gives the methods needed to encode context, but not the implementation. Other traits like [[SaveNoContext]] should be mixed in. The
 *   exact trait that is mixed in depends on the Context that you are using in your analysis.
 *
 * @tparam T
 *   The type of the value the needs to be saved
 */
trait SaveContext[Expr <: Expression] extends Save[Expr]:
    /** The type of context that should be encoded. */
    type EncodeContext

    /** Encodes context */
    given contextEncoder: Encoder[EncodeContext]

/**
 * Trait to encode the context for an analysis with no context.
 *
 * This will just write 'ε' when asked to write the context.
 *
 * @tparam T
 *   The type of the value the needs to be saved
 */
trait SaveNoContext[Expr <: Expression] extends SaveContext[Expr]:
    override type EncodeContext = NoContext.type
    override given contextEncoder: Encoder[EncodeContext] with
        override def write(writer: Writer, context: EncodeContext): Writer = writer.write("ε")

/**
 * Trait to encode standard scheme components.
 *
 * This is an implementation of [[SaveComponents]].
 */
trait SaveStandardSchemeComponents
    extends SaveActualComps[SchemeExp]
    with StandardSchemeModFComponents
    with SaveEnvironment[SchemeExp]
    with SaveContext[SchemeExp]
    with SaveExpressions[SchemeExp]:
    override type Component = SchemeModFComponent

    override given actualComponentEncoder: Encoder[Component] with
        def write(writer: Writer, component: Component): Writer =
            if component.equals(initialComponent) then writer.write("main")
            else writer.write(component.asInstanceOf[SchemeModFComponent.Call[ComponentContext]])

    given [T]: MapEncoder[SchemeModFComponent.Call[T]] with
        override def write(writer: Writer, component: SchemeModFComponent.Call[T]): Writer =
            writer.start()
            val (lambda, env) = component.clo
            val context = component.ctx
            writer.writeMember("lambda", lambda.asInstanceOf[SchemeExp])
            writer.writeMember("environment", env)
            writer.writeMember("context", context.asInstanceOf[EncodeContext])
            writer.close()

trait SaveWorklist[Expr <: Expression] extends Save[Expr] with SaveComponents[Expr]:
    given worklistEncoder: Encoder[WorkList[Component]]
    def getWorklist: WorkList[Component]
    override def saveInfo: List[(String, Savable[_])] = super.saveInfo ++ List(("worklist", Savable(getWorklist)))

trait SaveSequentialWorklist[Expr <: Expression] extends SaveWorklist[Expr] with SequentialWorklistAlgorithm[Expr]:
    override def getWorklist: WorkList[Component] = workList
    override given worklistEncoder: ArrayEncoder[WorkList[Component]] with
        override def write(writer: Writer, worklist: WorkList[Component]): Writer =
            writer.start()
            val worklistList = worklist.toList
            worklistList.foreach(writer.writeMember(_))
            writer.close()
