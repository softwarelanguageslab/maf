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
import io.bullet.borer.Decoder
import io.bullet.borer.Reader
import maf.save.EncapsulatedDecoder.*
import maf.core.Position
import maf.core.Position.PTag

trait SavePosition[Expr <: Expression] extends Save[Expr]:
    def getPositionEncoder: AbstractEncoder = getEncoder

    val posEncoder = getPositionEncoder
    given Encoder[Position] = AbstractEncoder.deriveAllEncoders[Position](posEncoder)
    given Encoder[PTag] = AbstractEncoder.deriveAllEncoders[PTag](posEncoder)
    given Encoder[Identifier] = AbstractEncoder.deriveEncoder[Identifier](posEncoder)
    given Encoder[Identity] = AbstractEncoder.deriveAllEncoders[Identity](posEncoder)
    given Encoder[IdentityData] with
        def write(writer: Writer, value: IdentityData): Writer =
            System.err.nn.println("IdentityData could not be encoded")
            writer

trait SaveComponents[Expr <: Expression] extends Save[Expr]:
    def getComponentEncoder: AbstractEncoder = getEncoder
    override def saveInfo: Map[String, Savable[_]] = super.saveInfo + ("components" -> Savable(visited))

    given componentEncoder: Encoder[Component]

trait SaveStandardSchemeComponentID extends StandardSchemeModFComponents with SavePosition[SchemeExp]:
    given componentIDEncoder: Encoder[Component] with
        def write(writer: Writer, component: Component): Writer =
            if component.equals(initialComponent) then writer.write("main")
            else writer.write(component.asInstanceOf[SchemeModFComponent.Call[ComponentContext]])(schemeComponentIDEncoder)

    given schemeComponentIDEncoder[T]: Encoder[SchemeModFComponent.Call[T]] with
        def write(writer: Writer, component: SchemeModFComponent.Call[T]): Writer =
            val (lambda, _) = component.clo
            writer.write(lambda.idn.pos)

trait SaveEnvironment[Expr <: Expression] extends Save[Expr] with SaveAddr[Expr]:
    def getEnvironmentEncoder: AbstractEncoder = getEncoder
    given [T <: Address]: EncapsulatedEncoder[Environment[T]] with
        override val encoder: AbstractEncoder = getEnvironmentEncoder
        override protected def writeEncapsulated(writer: Writer, env: Environment[T]): Writer =
            env match {
                case BasicEnvironment(content) =>
                    writer.writeMember("BasicEnvironment", content.asInstanceOf[Map[String, Address]])
                case NestedEnv(content, rst) =>
                    writer.open("NestedEnvironment")
                    writer.writeMember("content", content.asInstanceOf[Map[String, Address]])
                    if rst.isDefined then writer.writeMember("rst", rst.get.asInstanceOf[Address])
                    writer.close()
                case _ =>
                    System.err.nn.println("The environemnt with type `" + env.getClass + "` could not be encoded")
                    writer
            }

trait SaveContext[Expr <: Expression] extends Save[Expr]:
    type Context
    def getContextEncoder: AbstractEncoder = getEncoder
    given contextEncoder: Encoder[Context]

trait SaveNoContext[Expr <: Expression] extends SaveContext[Expr]:
    type Context = NoContext.type
    override given contextEncoder: Encoder[Context] with
        override def write(writer: Writer, context: Context): Writer = writer.write("ε")

trait SaveStandardSchemeComponents
    extends SaveComponents[SchemeExp]
    with StandardSchemeModFComponents
    with AnalysisResults[SchemeExp]
    with SaveValue[SchemeExp]
    with SavePosition[SchemeExp]
    with SaveEnvironment[SchemeExp]
    with SaveContext[SchemeExp]:

    given EncapsulatedEncoder[SchemeExp] with
        override val encoder = getComponentEncoder
        def writeEncapsulated(writer: Writer, exp: SchemeExp): Writer =
            exp match
                case funcall: SchemeFuncall        => writer.writeMember("funcall", funcall)
                case variable: SchemeVar           => writer.writeMember("var", variable)
                case lambda: SchemeLambda          => writer.writeMember("lambda", lambda)
                case argLambda: SchemeVarArgLambda => writer.writeMember("argLambda", argLambda)
                case _ =>
                    System.err.nn.println("The schemeexpression with type `" + exp.getClass + "` could not be encoded")
                    writer

    private val compEncoder = getComponentEncoder
    given Encoder[SchemeFuncall] = AbstractEncoder.deriveEncoder[SchemeFuncall](compEncoder)
    given Encoder[SchemeVar] = AbstractEncoder.deriveEncoder[SchemeVar](compEncoder)
    given Encoder[SchemeLambda] = AbstractEncoder.deriveEncoder[SchemeLambda](compEncoder)
    given Encoder[SchemeVarArgLambda] = AbstractEncoder.deriveEncoder[SchemeVarArgLambda](compEncoder)
    given Encoder[SchemeLambdaExp] = AbstractEncoder.deriveEncoder[SchemeLambdaExp](compEncoder)

    override given componentEncoder: Encoder[Component] with
        def write(writer: Writer, component: Component): Writer =
            if component.equals(initialComponent) then writer.write("main")
            else writer.write(component.asInstanceOf[SchemeModFComponent.Call[ComponentContext]])

    given [T]: EncapsulatedEncoder[SchemeModFComponent.Call[T]] with
        override val encoder = getComponentEncoder
        override def writeEncapsulated(writer: Writer, component: SchemeModFComponent.Call[T]): Writer =
            val (lambda, env) = component.clo
            val context = component.ctx
            writer.writeMember("lambda", lambda)
            writer.writeMember("environment", env)
            writer.writeMember("context", context.asInstanceOf[Context])

trait LoadComponents[Expr <: Expression] extends Load[Expr]:
    def getComponentDecoder: AbstractDecoder = getDecoder
    override def loadInfo: Map[String, Loadable[_]] =
        super.loadInfo + ("components" -> Loadable((visited: Set[Component]) =>
            visited.foreach((component) => if component != initialComponent then println(component.asInstanceOf[SchemeModFComponent.Call[_]].clo))
        ))

    given componentDecoder: Decoder[Component]

trait LoadStandardSchemeComponents
    extends LoadComponents[SchemeExp]
    with StandardSchemeModFComponents
    with LoadContext[SchemeExp]
    with LoadPosition[SchemeExp]
    with LoadEnvironment[SchemeExp]:
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
            return new SchemeModFComponent.Call[Context]((lambda.value.get.get, environment.value.get.get), context.value.get.get)
    private val compDecoder = getComponentDecoder
    given Decoder[SchemeFuncall] = AbstractDecoder.deriveDecoder[SchemeFuncall](compDecoder)
    given Decoder[SchemeVar] = AbstractDecoder.deriveDecoder[SchemeVar](compDecoder)
    given Decoder[SchemeLambda] = AbstractDecoder.deriveDecoder[SchemeLambda](compDecoder)
    given Decoder[SchemeVarArgLambda] = AbstractDecoder.deriveDecoder[SchemeVarArgLambda](compDecoder)
    given Decoder[SchemeLambdaExp] = AbstractDecoder.deriveDecoder[SchemeLambdaExp](compDecoder)

    given EncapsulatedDecoder[SchemeExp] with
        override val decoder = getComponentDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): SchemeExp =
            val expression = reader.readMembers[SchemeExp](
              Array(
                ("funcall", summon[Decoder[SchemeFuncall]]),
                ("var", summon[Decoder[SchemeVar]]),
                ("lambda", summon[Decoder[SchemeLambda]]),
                ("argLambda", summon[Decoder[SchemeVarArgLambda]])
              )
            )
            expression.value.get.get

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
    given [T <: Address]: EncapsulatedDecoder[Environment[T]] with
        override def decoder: AbstractDecoder = getEnvironmentDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Environment[T] =
            return reader
                .readMembers[Environment[T]](
                  Array(("BasicEnvironment", summon[Decoder[BasicEnvironment[T]]]), ("NestedEnv", summon[Decoder[NestedEnv[T, T]]]))
                )
                .value
                .get
                .get

    given [T <: Address]: Decoder[BasicEnvironment[T]] with
        override def read(reader: Reader): BasicEnvironment[T] = return new BasicEnvironment(
          reader.read[Map[String, Address]]().asInstanceOf[Map[String, T]]
        )
    given [T <: Address, K <: Address]: EncapsulatedDecoder[NestedEnv[T, K]] with
        override def decoder: AbstractDecoder = getEnvironmentDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): NestedEnv[T, K] =
            val content = reader.readMember[Map[String, Address]]("content")
            val rst = reader.readMember[Address]("rst")
            return new NestedEnv(content.value.get.get.asInstanceOf[Map[String, T]],
                                 if rst.isCompleted then Some(rst.value.get.get.asInstanceOf[K]) else None
            )
