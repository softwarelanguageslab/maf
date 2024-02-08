package maf.save

import io.bullet.borer.{Encoder, Writer}
import io.bullet.borer.Json
import maf.util.Writer.write
import maf.core.Expression
import maf.modular.ModAnalysis
import java.nio.file.Paths
import java.nio.file.Files
import maf.language.scheme.SchemeExp
import EncapsulatedEncoder.*

case class Savable[T](val value: T)(using val encoder: Encoder[T])

trait Save[Expr <: Expression] extends ModAnalysis[Expr]:
    def getEncoder: AbstractEncoder
    given EncapsulatedEncoder[Save[Expr]] with
        override val encoder = Save.this.getEncoder
        override def writeEncapsulated(writer: Writer, value: Save[Expr]): Writer =
            for (key, value) <- saveInfo do writer.writeMember(key, value.value)(using value.encoder, encoder)
            writer

    /**
     * This saves the current analysis to a file
     *
     * @param filename
     *   The file to save to
     */
    override def save(filename: String): Unit =
        val res = Json.encode(this).toByteArray
        Files.write(Paths.get(filename), res)

    def saveInfo: Map[String, Savable[_]] =
        Map("name" -> Savable(analysisName))

trait SaveModF
    extends Save[SchemeExp]
    with SaveStandardSchemeComponents
    with SaveModularDomain
    with SaveAddrDep
    with SaveSchemeAddr
    with SaveGlobalStore[SchemeExp]
    with SaveModularSchemeLattices
    with SaveNoContext[SchemeExp]:
    override def getEncoder: AbstractEncoder = new MapEncoder
