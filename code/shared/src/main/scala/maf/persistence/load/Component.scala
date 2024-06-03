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
import maf.core.Position
import maf.core.Position.PTag
import scala.collection.mutable.HashMap
import maf.language.scheme.SchemeValue
import maf.language.scheme.SchemeLetrec
import maf.language.scheme.SchemeAssert
import maf.language.scheme.SchemeSet
import maf.language.scheme.SchemeBegin
import maf.language.scheme.SchemeLetStar
import maf.core.worklist.WorkList
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.modular.worklist.FIFOWorklistAlgorithm
import scala.collection.immutable.Queue
import maf.core.worklist.FIFOWorkList
import maf.modular.ModAnalysis
import io.bullet.borer.derivation.CompactMapBasedCodecs

/**
 * The base trait for decoding components.
 *
 * @note
 *   This trait gives the methods needed to decode components, but does not implement them yet, other traits like [[LoadStandardSchemeComponents]] or
 *   [[LoadComponentIntID]] should be mixed in for the implementation. The trait that should be mixed in depends on the kind of components are used in
 *   your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadComponents[Expr <: Expression] extends ModAnalysis[Expr] with Load[Expr]:
    given componentDecoder: Decoder[Component]

/**
 * The base trait for decoding components.
 *
 * This trait is used to add [[actualComponentDecoder]], this given cannot be added into [[LoadComponents]] because this would cause an ambigious
 * implicit with [[componentEncoder]].
 *
 * @note
 *   This trait gives the methods needed to decode components, but does not implement them yet, other traits like [[LoadStandardSchemeComponents]] or
 *   [[LoadComponentIntID]] should be mixed in for the implementation. The trait that should be mixed in depends on the kind of components are used in
 *   your analysis.
 * @note
 *   This trait should not be used, rather, [[LoadExpressions]] should be extended.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
protected trait LoadActualComps[Expr <: Expression] extends LoadComponents[Expr]:
    /** Encodes the actual component. */
    given actualComponentDecoder: Decoder[Component]

/**
 * Load components normally.
 *
 * Implementation of [[LoadComponents]]
 */
trait LoadActualComponents[Expr <: Expression] extends LoadActualComps[Expr]:
    override given componentDecoder: Decoder[Component] with
        override def read(reader: Reader): Component =
            val component = reader.read[Component]()(using actualComponentDecoder)
            if !visited.contains(component) then visited = visited + component
            return component

/**
 * Load standard scheme components
 *
 * Implementation of [[LoadComponents]]
 */
trait LoadStandardSchemeComponents
    extends LoadActualComps[SchemeExp]
    with StandardSchemeModFComponents
    with LoadContext[SchemeExp]
    with LoadEnvironment[SchemeExp]
    with LoadExpressions[SchemeExp]:
    override given actualComponentDecoder: MapDecoder[Component] with
        override def read(reader: Reader): Component =
            if reader.tryReadString("main") then return initialComponent
            else reader.read[SchemeModFComponent.Call[DecodeContext]]()

    given MapDecoder[SchemeModFComponent.Call[DecodeContext]] with
        override def read(reader: Reader): SchemeModFComponent.Call[DecodeContext] =
            reader.start()
            val lambda = reader.readMember[SchemeExp]("lambda").asInstanceOf[ReadValue[String, SchemeLambdaExp]]
            val environment = reader.readMember[Environment[Address]]("environment")
            val context = reader.readMember[DecodeContext]("context")
            reader.close()
            return new SchemeModFComponent.Call[DecodeContext]((lambda.value, environment.value), context.value)

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
 * This will expect 'None' when reading context.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadNoContext[Expr <: Expression] extends LoadContext[Expr]:
    override type DecodeContext = NoContext.type
    override given contextDecoder: Decoder[DecodeContext] with
        override def read(reader: Reader): DecodeContext =
            if !reader.tryReadString("None") then return reader.unexpectedDataItem("None")
            return NoContext

/**
 * Trait to decode positions.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadPosition[Expr <: Expression] extends Load[Expr]:
    given Decoder[Position] = CompactMapBasedCodecs.deriveAllDecoders[Position]
    given Decoder[PTag] = CompactMapBasedCodecs.deriveAllDecoders[PTag]

    given Decoder[Identifier] = CompactMapBasedCodecs.deriveDecoder[Identifier]
    given Decoder[Identity] = CompactMapBasedCodecs.deriveAllDecoders[Identity]
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
    given [T <: Address]: MapDecoder[Environment[T]] with
        override def read(reader: Reader): Environment[T] =
            reader.start()
            val value = reader
                .readMembers[Environment[T]](
                  Array(("basicEnvironment", summon[Decoder[BasicEnvironment[T]]]), ("nestedEnvironment", summon[Decoder[NestedEnv[T, T]]]))
                )
                .value
            reader.close()
            return value

    given [T <: Address]: Decoder[BasicEnvironment[T]] with
        override def read(reader: Reader): BasicEnvironment[T] = return new BasicEnvironment(
          reader.read[Map[String, Address]]().asInstanceOf[Map[String, T]]
        )

    given [T <: Address, K <: Address]: MapDecoder[NestedEnv[T, K]] with
        override def read(reader: Reader): NestedEnv[T, K] =
            reader.start()
            val content = reader.readMember[Map[String, Address]]("content")
            val rst = reader.readOptionMember[Address]("rst")
            reader.close()
            return new NestedEnv(content.value.asInstanceOf[Map[String, T]], rst.value.asInstanceOf[Option[K]])

/**
 * The base trait for decoding components only by their ID.
 *
 * @note
 *   This trait gives the methods needed to decode context, but does not implement them yet, other traits like [[LoadComponentIntID]] should be mixed
 *   in for the implementation. The trait that should be mixed in depends on the kind of context that is used in your analysis.
 *
 * @note
 *   Because this trait only decodes the component IDs, the entire component should have already been decoded and placed in [[components]], so the ID
 *   can be mapped to an actual component.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadComponentID[Expr <: Expression] extends LoadActualComps[Expr] with LoadPosition[Expr]:
    /* The type of ID that will be used to load components */
    type ID

    /** Map that connects component IDs to the component. */
    var components = HashMap[ID, Component]()

    override def startLoad(): Unit =
        super.startLoad()
        components = HashMap[ID, Component]()

    /**
     * Register a loaded component, this allows you to also reference components using their ID using the [[components]] map.
     *
     * @param component
     *   The component to register
     */
    def addComponent(component: Component): Unit
    override given componentDecoder: Decoder[Component] = componentIDDecoder
    given componentIDDecoder: Decoder[Component]
    protected given componentSetDecoder: Decoder[Set[Component]]

    override def loadInfo: List[(String, Loadable[_])] =
        super.loadInfo ++ List(("components" -> Loadable((visited: Set[Component]) =>
            visited.foreach((component) => addComponent(component))
            this.visited = visited
        )))

/**
 * Trait that decodes standard scheme components using their position.
 *
 * Implementation of [[LoadComponentID]].
 *
 * @note
 *   Because this trait only decodes the component positions, the entire component should have already been decoded and placed in [[components]], so
 *   the position can be mapped to an actual component.
 */
trait LoadStandardSchemeComponentPosition extends LoadComponentID[SchemeExp] with LoadContext[SchemeExp]:
    override type ID = Position
    override def addComponent(component: Component): Unit =
        if component != initialComponent then
            components.addOne(component.asInstanceOf[SchemeModFComponent.Call[DecodeContext]].clo._1.idn.pos, component)

    override protected given componentSetDecoder: ArrayDecoder[Set[Component]] with
        override def read(reader: Reader): Set[Component] =
            reader.start()
            val components =
                reader.readUntilBeforeBreak(
                  Set[Component](),
                  (components: Set[Component]) => components + reader.readMember[Component]()(using actualComponentDecoder).value
                )
            reader.close()
            return components

    override given componentIDDecoder: Decoder[Component] with
        override def read(reader: Reader): Component =
            if reader.tryReadString("main") then return initialComponent
            else reader.read[SchemeModFComponent.Call[DecodeContext]]().asInstanceOf[Component]

    given schemeComponentIDDecoder[T]: Decoder[SchemeModFComponent.Call[DecodeContext]] with
        override def read(reader: Reader): SchemeModFComponent.Call[DecodeContext] =
            val pos = reader.read[Position]()
            return components(pos).asInstanceOf[SchemeModFComponent.Call[DecodeContext]]

/**
 * Trait that decodes standard scheme components using an integer ID.
 *
 * Implementation of [[LoadComponentID]].
 *
 * @note
 *   Because this trait only decodes the component ID, the entire component should have already been decoded and placed in [[components]], so the ID
 *   can be mapped to an actual component.
 */
trait LoadComponentIntID[Expr <: Expression] extends LoadComponentID[Expr]:
    override type ID = Int
    override def addComponent(component: Component): Unit = return

    override given componentIDDecoder: Decoder[Component] with
        override def read(reader: Reader): Component =
            if reader.hasInt then
                val id = reader.readInt()
                return components(id)
            else return reader.read[Component]()(using actualComponentDecoder)

    override protected given componentSetDecoder: MapDecoder[Set[Component]] with
        override def read(reader: Reader): Set[Component] =
            reader.start()
            val components = reader.readUntilBeforeBreak(
              Set[Component](),
              (components: Set[Component]) =>
                  val component = reader.readMember[Component]()(using componentDecoder)
                  val key = component.key.toInt
                  LoadComponentIntID.this.components.addOne((key, component.value))
                  components + (component.value)
            )
            reader.close()
            return components

trait LoadWorklist[Expr <: Expression] extends ModAnalysis[Expr] with Load[Expr]:
    given worklistDecoder: Decoder[WorkList[Component]]
    def setWorklist(worklist: WorkList[Component]): Unit
    def newWorklist(components: List[Component]): WorkList[Component]
    override def loadInfo: List[(String, Loadable[_])] =
        super.loadInfo ++ List(("worklist" -> Loadable((worklist: WorkList[Component]) => setWorklist(worklist))))

trait LoadSequentialWorklist[Expr <: Expression] extends SequentialWorklistAlgorithm[Expr] with LoadWorklist[Expr] with LoadComponents[Expr]:
    override def setWorklist(worklist: WorkList[Component]): Unit = workList = worklist
    given worklistDecoder: ArrayDecoder[WorkList[Component]] with
        override def read(reader: Reader): WorkList[Component] =
            reader.start()
            val worklistComponents = reader.readUntilBeforeBreak(List[Component](), (lst: List[Component]) => lst ++ List(reader.read[Component]()))
            reader.close()
            return newWorklist(worklistComponents)

trait LoadFIFOWorklist[Expr <: Expression] extends LoadSequentialWorklist[Expr] with FIFOWorklistAlgorithm[Expr]:
    override def newWorklist(components: List[Component]): WorkList[Component] =
        return FIFOWorkList[Component](components.iterator.to(Iterable))
