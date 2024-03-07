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
import EncapsulatedEncoder.*
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

/**
 * Trait to encode positions.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SavePosition[Expr <: Expression] extends Save[Expr]:
    /**
     * Get the encoder that will be used to encode positions.
     *
     * This will influence how positions will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getPositionEncoder: AbstractEncoder = getEncoder

    private val posEncoder = getPositionEncoder
    given Encoder[Position] = AbstractEncoder.deriveAllEncoders[Position](posEncoder)
    given Encoder[PTag] = AbstractEncoder.deriveAllEncoders[PTag](posEncoder)
    given Encoder[Identifier] = AbstractEncoder.deriveEncoder[Identifier](posEncoder)
    given Encoder[Identity] = AbstractEncoder.deriveAllEncoders[Identity](posEncoder)
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
trait SaveComponents[Expr <: Expression] extends Save[Expr]:
    /**
     * Get the encoder that will be used to encode components.
     *
     * This will influence how components will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getComponentEncoder: AbstractEncoder = getEncoder

    /**
     * Get the encoder that will be used to encode components.
     *
     * This encoder is used to encode objects where the key is important, when you e.g. encode a type in the key, some encoders might remove this key,
     * and should therefore not be used here.
     *
     * This will influence how components will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getComponentKeyEncoder: AbstractEncoder = getKeyEncoder
    override def saveInfo: List[(String, Savable[_])] = super.saveInfo ++ List(("components", Savable(visited)))

    /** Encodes a component. */
    given componentEncoder: Encoder[Component]

    /** Encodes a set of components */
    protected given componentSetEncoder: Encoder[Set[Component]]

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
trait SaveComponentID[Expr <: Expression] extends SavePosition[Expr]:
    /** Encodes a component by their ID */
    given componentIDEncoder: Encoder[Component]

/**
 * Trait that encodes components using an autoincreasing integer ID.
 *
 * Implementation of [[SaveComponentID]]
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveComponentIntID[Expr <: Expression] extends SaveComponents[Expr] with SaveComponentID[Expr]:
    private val components = HashMap[Component, Int]()
    private var id = 0

    override protected given componentSetEncoder: EncapsulatedEncoder[Set[Component]] with
        override val encoder: AbstractEncoder = getComponentKeyEncoder
        override protected def writeEncapsulated(writer: Writer, components: Set[Component]): Writer =
            for (component <- components) do
                SaveComponentIntID.this.components.addOne((component, id))
                writer.writeMember(id.toString(), component)(using componentEncoder, encoder)
                id += 1
            writer

    override given componentIDEncoder: Encoder[Component] with
        override def write(writer: Writer, component: Component): Writer = writer.write(components(component))

/**
 * Trait that encodes components using their position.
 *
 * Implementation of [[SaveComponentID]]
 *
 * @note
 *   Because this trait only encodes the component position, the entire component should be encoded somewhere else if you want to decode this again.
 */
trait SaveStandardSchemeComponentPosition extends SaveComponents[SchemeExp] with SaveComponentID[SchemeExp] with StandardSchemeModFComponents:
    override type Component = SchemeModFComponent

    /**
     * Get the encoder that will be used to encode a set of components.
     *
     * This will influence how a set of components will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an
     * [[ArrayEncoder array-based]] encoder.
     */
    def getComponentSetEncoder: AbstractEncoder = new ArrayEncoder

    /** Encodes a component by their position */
    override given componentIDEncoder: Encoder[Component] with
        def write(writer: Writer, component: Component): Writer =
            if component.equals(initialComponent) then writer.write("main")
            else writer.write(component.asInstanceOf[SchemeModFComponent.Call[ComponentContext]])(schemeComponentIDEncoder)

    /** Encodes a scheme component using their position */
    given schemeComponentIDEncoder[T]: Encoder[SchemeModFComponent.Call[T]] with
        def write(writer: Writer, component: SchemeModFComponent.Call[T]): Writer =
            val (lambda, _) = component.clo
            writer.write(lambda.idn.pos)

    override protected given componentSetEncoder: EncapsulatedEncoder[Set[Component]] with
        override val encoder: AbstractEncoder = getComponentSetEncoder
        override protected def writeEncapsulated(writer: Writer, components: Set[Component]): Writer =
            for (component <- components) do writer.writeMember(component)
            writer

/**
 * Trait to encode environments.
 *
 * @tparam T
 *   The type of the value the needs to be saved
 */
trait SaveEnvironment[Expr <: Expression] extends Save[Expr] with SaveAddr[Expr]:
    /**
     * Get the encoder that will be used to encode environments.
     *
     * This will influence how environments will be encodes, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getEnvironmentEncoder: AbstractEncoder = getEncoder

    /**
     * Get the encoder that will be used to encode environments.
     *
     * This encoder is used to encode objects where the key is important, when you e.g. encode a type in the key, some encoders might remove this key,
     * and should therefore not be used here.
     *
     * This will influence how environments will be encodes, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getEnvironmentKeyEncoder: AbstractEncoder = getKeyEncoder
    given Encoder[BasicEnvironment[Address]] with
        override def write(writer: Writer, env: BasicEnvironment[Address]): Writer = writer.write(env.content)

    given EncapsulatedEncoder[NestedEnv[Address, Address]] with
        override val encoder: AbstractEncoder = getEnvironmentEncoder
        override protected def writeEncapsulated(writer: Writer, env: NestedEnv[Address, Address]): Writer =
            writer.writeMember("content", env.content)
            if env.rst.isDefined then writer.writeMember("rst", env.rst.get)
            writer

    given EncapsulatedEncoder[Environment[Address]] with
        override val encoder: AbstractEncoder = getEnvironmentKeyEncoder
        override protected def writeEncapsulated(writer: Writer, env: Environment[Address]): Writer =
            env match {
                case basicEnv @ BasicEnvironment(_) =>
                    writer.writeMember("basicEnvironment", basicEnv)
                case nestedEnv @ NestedEnv(_, _) =>
                    writer.writeMember("nestedEnvironment", nestedEnv.asInstanceOf[NestedEnv[Address, Address]])
                case _ =>
                    System.err.nn.println("The environemnt with type `" + env.getClass + "` could not be encoded")
                    writer
            }

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

    /**
     * Get the encoder that will be used to encode context.
     *
     * This will influence how context will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getContextEncoder: AbstractEncoder = getEncoder

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
    extends SaveComponents[SchemeExp]
    with StandardSchemeModFComponents
    with AnalysisResults[SchemeExp]
    with SaveValue[SchemeExp]
    with SavePosition[SchemeExp]
    with SaveEnvironment[SchemeExp]
    with SaveContext[SchemeExp]
    with SaveExpressions[SchemeExp]:
    override type Component = SchemeModFComponent

    override given componentEncoder: Encoder[Component] with
        def write(writer: Writer, component: Component): Writer =
            if component.equals(initialComponent) then writer.write("main")
            else writer.write(component.asInstanceOf[SchemeModFComponent.Call[ComponentContext]])

    given [T]: EncapsulatedEncoder[SchemeModFComponent.Call[T]] with
        override val encoder = getComponentEncoder
        override def writeEncapsulated(writer: Writer, component: SchemeModFComponent.Call[T]): Writer =
            val (lambda, env) = component.clo
            val context = component.ctx
            writer.writeMember("lambda", lambda.asInstanceOf[SchemeExp])
            writer.writeMember("environment", env)
            writer.writeMember("context", context.asInstanceOf[EncodeContext])
