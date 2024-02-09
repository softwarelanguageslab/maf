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

trait LoadComponents[Expr <: Expression] extends Load[Expr] with LoadComponentID[Expr]:
    def getComponentDecoder: AbstractDecoder = getDecoder
    def getComponentKeyDecoder: AbstractDecoder = getKeyDecoder
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
        if component != initialComponent then components.addOne(component.asInstanceOf[SchemeModFComponent.Call[Context]].clo._1.idn.pos, component)

    override given componentDecoder: Decoder[Component] with
        override def read(reader: Reader): Component =
            if reader.tryReadString("main") then return initialComponent
            else reader.read[SchemeModFComponent.Call[Context]]()

    given EncapsulatedDecoder[SchemeModFComponent.Call[Context]] with
        override val decoder = getComponentDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): SchemeModFComponent.Call[Context] =
            val lambda = reader.readMember[SchemeLambdaExp]("lambda")
            val environment = reader.readMember[Environment[Address]]("environment")
            val context = reader.readMember[Context]("context")
            return new SchemeModFComponent.Call[Context]((lambda.value, environment.value), context.value)
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

trait LoadContext[Expr <: Expression] extends Load[Expr]:
    type Context
    def getContextDecoder: AbstractDecoder = getDecoder
    given contextDecoder: Decoder[Context]

trait LoadNoContext[Expr <: Expression] extends LoadContext[Expr]:
    type Context = NoContext.type
    override given contextDecoder: Decoder[Context] with
        override def read(reader: Reader): Context =
            if !reader.tryReadString("ε") then return reader.unexpectedDataItem("ε")
            NoContext

trait LoadPosition[Expr <: Expression] extends Load[Expr]:
    def getPositionDecoder: AbstractDecoder = getDecoder
    given Decoder[Position] = AbstractDecoder.deriveAllDecoders[Position](getPositionDecoder)
    given Decoder[PTag] = AbstractDecoder.deriveAllDecoders[PTag](getPositionDecoder)

    val posDecoder = getPositionDecoder
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

trait LoadEnvironment[Expr <: Expression] extends Load[Expr] with LoadAddr[Expr]:
    def getEnvironmentDecoder: AbstractDecoder = getDecoder
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

trait LoadComponentID[Expr <: Expression] extends LoadPosition[Expr]:
    var components = HashMap[Position, Component]()
    given componentIDDecoder: Decoder[Component]

trait LoadStandardSchemeComponentID extends LoadComponentID[SchemeExp] with LoadContext[SchemeExp]:
    given componentIDDecoder: Decoder[Component] with
        override def read(reader: Reader): Component =
            if reader.tryReadString("main") then return initialComponent
            else reader.read[SchemeModFComponent.Call[Context]]().asInstanceOf[Component]

    given schemeComponentIDDecoder[T]: Decoder[SchemeModFComponent.Call[Context]] with
        override def read(reader: Reader): SchemeModFComponent.Call[Context] =
            val pos = reader.read[Position]()
            return components(pos).asInstanceOf[SchemeModFComponent.Call[Context]]
