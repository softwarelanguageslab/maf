package maf.save

import io.bullet.borer.{Decoder, Encoder, Writer}
import io.bullet.borer.Json
import maf.util.Writer.write
import maf.core.Expression
import maf.modular.ModAnalysis
import java.nio.file.Paths
import java.nio.file.Files
import maf.language.scheme.SchemeExp

class Savable[T](val value: T)(using val encoder: Encoder[T])

trait Save[Expr <: Expression] extends ModAnalysis[Expr]:
    given Encoder[Save[Expr]] with
        override def write(writer: Writer, value: Save[Expr]): Writer =
            writer.writeMapStart()
            for (key, value) <- saveInfo do writer.writeMapMember(key, value.value)(using summon[Encoder[String]], value.encoder)
            writer.writeMapClose()

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

trait SaveModF extends Save[SchemeExp]
