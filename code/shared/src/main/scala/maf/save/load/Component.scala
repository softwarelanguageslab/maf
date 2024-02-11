package maf.save

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
import io.bullet.borer.Decoder
import io.bullet.borer.Reader
import maf.save.EncapsulatedDecoder.*
import maf.core.Position
import maf.core.Position.PTag
import scala.collection.mutable.HashMap

/**
 * The base trait for decoding components.
 *
 * @note
 *   This trait gives the methods needed to decode components, but does not implement them yet, other traits like [[LoadStandardSchemeComponents]] or
 *   [[LoadStandardSchemeComponentID]] should be mixed in for the implementation. The trait that should be mixed in depends on the kind of components
 *   are used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadComponents[Expr <: Expression] extends Load[Expr] with LoadComponentID[Expr]:
    /**
     * Get the decoder that will be used to decode components.
     *
     * This will influence how components will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getComponentDecoder: AbstractDecoder = getDecoder

    /**
     * Get the decoder that will be used to decode components.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how components will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getComponentKeyDecoder: AbstractDecoder = getKeyDecoder

    /**
     * Register a loaded component, this is necessary if you also want to load components by their ID.
     *
     * @param component
     *   The component to register
     */
    def addComponent(component: Component): Unit
    override def loadInfo: Map[String, Loadable[_]] =
        super.loadInfo + ("components" -> Loadable((visited: Set[Component]) =>
            visited.foreach((component) => addComponent(component))
            this.visited = visited
        ))

    given componentDecoder: Decoder[Component]

trait LoadStandardSchemeComponents
    extends LoadComponents[SchemeExp]
    with StandardSchemeModFComponents
    with LoadContext[SchemeExp]
    with LoadPosition[SchemeExp]
    with LoadEnvironment[SchemeExp]:
    override def addComponent(component: SchemeModFComponent): Unit =
        if component != initialComponent then
            components.addOne(component.asInstanceOf[SchemeModFComponent.Call[DecodeContext]].clo._1.idn.pos, component)

    override given componentDecoder: Decoder[Component] with
        override def read(reader: Reader): Component =
            if reader.tryReadString("main") then return initialComponent
            else reader.read[SchemeModFComponent.Call[DecodeContext]]()

    given EncapsulatedDecoder[SchemeModFComponent.Call[DecodeContext]] with
        override val decoder = getComponentDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): SchemeModFComponent.Call[DecodeContext] =
            val lambda = reader.readMember[SchemeLambdaExp]("lambda")
            val environment = reader.readMember[Environment[Address]]("environment")
            val context = reader.readMember[DecodeContext]("context")
            return new SchemeModFComponent.Call[DecodeContext]((lambda.value, environment.value), context.value)
    private val compDecoder = getComponentDecoder
    given Decoder[SchemeFuncall] = AbstractDecoder.deriveDecoder[SchemeFuncall](compDecoder)
    given Decoder[SchemeVar] = AbstractDecoder.deriveDecoder[SchemeVar](compDecoder)
    given Decoder[SchemeLambda] = AbstractDecoder.deriveDecoder[SchemeLambda](compDecoder)
    given Decoder[SchemeVarArgLambda] = AbstractDecoder.deriveDecoder[SchemeVarArgLambda](compDecoder)
    given Decoder[SchemeLambdaExp] = AbstractDecoder.deriveDecoder[SchemeLambdaExp](compDecoder)

    given EncapsulatedDecoder[SchemeExp] with
        override val decoder = getComponentKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): SchemeExp =
            val expression = reader.readMembers[SchemeExp](
              Array(
                ("funcall", summon[Decoder[SchemeFuncall]]),
                ("var", summon[Decoder[SchemeVar]]),
                ("lambda", summon[Decoder[SchemeLambda]]),
                ("argLambda", summon[Decoder[SchemeVarArgLambda]])
              )
            )
            expression.value

/**
 * The base trait for decoding context.
 *
 * @note
 *   This trait gives the methods needed to decode context, but does not implement them yet, other traits like [[LoadNoContext]] should be mixed in
 *   for the implementation. The trait that should be mixed in depends on the kind of context that is used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadContext[Expr <: Expression] extends Load[Expr]:
    /** The type of context that should be decoded. */
    type DecodeContext

    /**
     * Get the decoder that will be used to decode context.
     *
     * This will influence how context will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getContextDecoder: AbstractDecoder = getDecoder

    /**
     * Get the decoder that will be used to decode context.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how context will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    given contextDecoder: Decoder[DecodeContext]

/**
 * Trait to decode the context for an analysis with no context.
 *
 * This will expect 'ε' when reading context.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadNoContext[Expr <: Expression] extends LoadContext[Expr]:
    override type DecodeContext = NoContext.type
    override given contextDecoder: Decoder[DecodeContext] with
        override def read(reader: Reader): DecodeContext =
            if !reader.tryReadString("ε") then return reader.unexpectedDataItem("ε")
            NoContext

/**
 * Trait to decode positions.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadPosition[Expr <: Expression] extends Load[Expr]:
    /**
     * Get the decoder that will be used to decode positions.
     *
     * This will influence how positions will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getPositionDecoder: AbstractDecoder = getDecoder
    given Decoder[Position] = AbstractDecoder.deriveAllDecoders[Position](getPositionDecoder)
    given Decoder[PTag] = AbstractDecoder.deriveAllDecoders[PTag](getPositionDecoder)

    private val posDecoder = getPositionDecoder
    given Decoder[Identifier] = AbstractDecoder.deriveDecoder[Identifier](posDecoder)
    given Decoder[Identity] = AbstractDecoder.deriveAllDecoders[Identity](posDecoder)
    given Decoder[IdentityData] with
        private object IdnData extends IdentityData {
            // TODO:
            def canEqual(that: Any): Boolean = ???
            def productArity: Int = ???
            def productElement(n: Int): Any = ???
        }
        override def read(reader: Reader): IdentityData =
            System.err.nn.println("IdentityData could not be decoded")
            return IdnData

/**
 * Trait to decode environments.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadEnvironment[Expr <: Expression] extends Load[Expr] with LoadAddr[Expr]:
    /**
     * Get the decoder that will be used to decode environments.
     *
     * This will influence how environments will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getEnvironmentDecoder: AbstractDecoder = getDecoder

    /**
     * Get the decoder that will be used to decode environments.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how environments will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getEnvironmentKeyDecoder: AbstractDecoder = getKeyDecoder
    given [T <: Address]: EncapsulatedDecoder[Environment[T]] with
        override def decoder: AbstractDecoder = getEnvironmentKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Environment[T] =
            return reader
                .readMembers[Environment[T]](
                  Array(("basicEnvironment", summon[Decoder[BasicEnvironment[T]]]), ("nestedEnv", summon[Decoder[NestedEnv[T, T]]]))
                )
                .value

    given [T <: Address]: Decoder[BasicEnvironment[T]] with
        override def read(reader: Reader): BasicEnvironment[T] = return new BasicEnvironment(
          reader.read[Map[String, Address]]().asInstanceOf[Map[String, T]]
        )
    given [T <: Address, K <: Address]: EncapsulatedDecoder[NestedEnv[T, K]] with
        override def decoder: AbstractDecoder = getEnvironmentDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): NestedEnv[T, K] =
            val content = reader.readMember[Map[String, Address]]("content")
            val rst = reader.readMember[Address]("rst")
            return new NestedEnv(content.value.asInstanceOf[Map[String, T]], if rst.hasValue then Some(rst.value.asInstanceOf[K]) else None)

/**
 * The base trait for decoding components only by their ID.
 *
 * @note
 *   This trait gives the methods needed to decode context, but does not implement them yet, other traits like [[LoadStandardSchemeComponentID]]
 *   should be mixed in for the implementation. The trait that should be mixed in depends on the kind of context that is used in your analysis.
 *
 * @note
 *   Because this trait only decodes the component IDs, the entire component should have already been decoded and placed in [[components]], so the ID
 *   can be mapped to an actual component.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadComponentID[Expr <: Expression] extends LoadPosition[Expr]:
    /** Map that connects component IDs to the component. */
    var components = HashMap[Position, Component]()
    given componentIDDecoder: Decoder[Component]

/**
 * Trait that decodes components using their position.
 *
 * Implementation of [[LoadComponentID]].
 *
 * @note
 *   Because this trait only decodes the component positions, the entire component should have already been decoded and placed in [[components]], so
 *   the position can be mapped to an actual component.
 */
trait LoadStandardSchemeComponentID extends LoadComponentID[SchemeExp] with LoadContext[SchemeExp]:
    given componentIDDecoder: Decoder[Component] with
        override def read(reader: Reader): Component =
            if reader.tryReadString("main") then return initialComponent
            else reader.read[SchemeModFComponent.Call[DecodeContext]]().asInstanceOf[Component]

    given schemeComponentIDDecoder[T]: Decoder[SchemeModFComponent.Call[DecodeContext]] with
        override def read(reader: Reader): SchemeModFComponent.Call[DecodeContext] =
            val pos = reader.read[Position]()
            return components(pos).asInstanceOf[SchemeModFComponent.Call[DecodeContext]]
