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
