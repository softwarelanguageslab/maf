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

trait SavePosition[Expr <: Expression] extends Save[Expr]:
    given MapEncoder[Position] with
        override def writeEncapsulated(writer: Writer, pos: Position): Writer =
            writer.writeMember("line", pos.line)
            writer.writeMember("col", pos.line)
            if !pos.tag.show.isEmpty() then writer.writeMember("tag", pos.tag.show)
            writer

trait SaveComponents[Expr <: Expression] extends Save[Expr]:
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

trait SaveStandardSchemeComponents
    extends SaveComponents[SchemeExp]
    with StandardSchemeModFComponents
    with AnalysisResults[SchemeExp]
    with SaveValue[SchemeExp]
    with SavePosition[SchemeExp]:

    given MapEncoder[SchemeExp] with
        def writeEncapsulated(writer: Writer, exp: SchemeExp): Writer =
            val stringEncoder = summon[Encoder[String]]
            exp match
                case funcall: SchemeFuncall =>
                    writer.writeMember("type", "funcall")(using stringEncoder, stringEncoder, this)
                    writer.writeMember("expression", funcall)(using summon[Encoder[String]], summon[Encoder[SchemeFuncall]], this)
                case variable: SchemeVar =>
                    writer.writeMember("type", "var")(using stringEncoder, stringEncoder, this)
                    writer.writeMember("expression", variable)(using summon[Encoder[String]], summon[Encoder[SchemeVar]], this)
                case lambda: SchemeLambda =>
                    writer.writeMember("type", "lambda")(using stringEncoder, stringEncoder, this)
                    writer.writeMember("expression", lambda)(using summon[Encoder[String]], summon[Encoder[SchemeLambda]], this)
                case argLambda: SchemeVarArgLambda =>
                    writer.writeMember("type", "argLambda")(using stringEncoder, stringEncoder, this)
                    writer.writeMember("expression", argLambda)(using summon[Encoder[String]], summon[Encoder[SchemeVarArgLambda]], this)
                case _ =>
                    System.err.nn.println("The schemeexpression with type `" + exp.getClass + "` could not be encoded")
                    writer

    given Encoder[SchemeFuncall] = MapBasedCodecs.deriveEncoder[SchemeFuncall]
    given Encoder[SchemeVar] = MapBasedCodecs.deriveEncoder[SchemeVar]
    given Encoder[SchemeLambda] = MapBasedCodecs.deriveEncoder[SchemeLambda]
    given Encoder[SchemeVarArgLambda] = MapBasedCodecs.deriveEncoder[SchemeVarArgLambda]
    given Encoder[SchemeLambdaExp] = MapBasedCodecs.deriveEncoder[SchemeLambdaExp]
    given Encoder[Identifier] = MapBasedCodecs.deriveEncoder[Identifier]
    given Encoder[Identity] = MapBasedCodecs.deriveAllEncoders[Identity]
    given Encoder[IdentityData] with
        def write(writer: Writer, value: IdentityData): Writer =
            System.err.nn.println("IdentityData could not be encoded")
            writer

    override given componentEncoder: Encoder[Component] with
        def write(writer: Writer, component: Component): Writer =
            if component.equals(initialComponent) then writer.write("main")
            else writer.write(component.asInstanceOf[SchemeModFComponent.Call[ComponentContext]])

    given [T]: MapEncoder[SchemeModFComponent.Call[T]] with
        override def writeEncapsulated(writer: Writer, component: SchemeModFComponent.Call[T]): Writer =
            val (lambda, env) = component.clo
            val context = component.ctx
            writer.writeMember("lambda", lambda)(using summon[Encoder[String]], summon[Encoder[SchemeLambdaExp]], this)
