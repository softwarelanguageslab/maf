package maf.save

import io.bullet.borer.{Decoder, Encoder, Writer}
import io.bullet.borer.Json
import maf.util.Writer.write
import maf.core.Expression
import maf.modular.ModAnalysis
import java.nio.file.Paths
import java.nio.file.Files
import maf.language.scheme.SchemeExp
import EncapsulatedEncoder.*
import io.bullet.borer.Reader
import maf.save.EncapsulatedDecoder.*

case class Savable[T](val value: T)(using val encoder: Encoder[T])
case class Loadable[T](val load: (T) => Unit)(using val decoder: Decoder[T])

trait Load[Expr <: Expression] extends ModAnalysis[Expr]:
    def getDecoder: AbstractDecoder = new MapDecoder

    given EncapsulatedDecoder[Load[Expr]] with
        override val decoder: AbstractDecoder = Load.this.getDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Load[Expr] =
            for (key, value) <- loadInfo do reader.readMember(key)(using value.decoder, decoder)
            for (key, value) <- loadInfo do value.load(reader.getMember(key))
            return Load.this

    override def load(filename: String): Unit =
        val bytes = Files.readAllBytes(Paths.get(filename))
        if bytes != null then Json.decode(bytes).to[Load[Expr]].value

    def loadInfo: Map[String, Loadable[_]] =
        Map("name" -> Loadable((name: String) => println(name)))

trait Save[Expr <: Expression] extends ModAnalysis[Expr]:
    def getEncoder: AbstractEncoder
    given EncapsulatedEncoder[Save[Expr]] with
        override val encoder = Save.this.getEncoder
        override def writeEncapsulated(writer: Writer, value: Save[Expr]): Writer =
            for (key, value) <- saveInfo do writer.writeMember(key, value.value)(using summon[Encoder[String]], value.encoder, encoder)
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

trait LoadModF extends Load[SchemeExp]
