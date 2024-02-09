package maf.save

import io.bullet.borer.Decoder
import io.bullet.borer.Json
import maf.core.Expression
import maf.modular.ModAnalysis
import java.nio.file.Paths
import java.nio.file.Files
import maf.language.scheme.SchemeExp
import io.bullet.borer.Reader
import maf.save.EncapsulatedDecoder.*

case class Loadable[T](val load: (T) => Unit)(using val decoder: Decoder[T])

trait Load[Expr <: Expression] extends ModAnalysis[Expr]:
    def getDecoder: AbstractDecoder
    def getKeyDecoder: AbstractDecoder

    given EncapsulatedDecoder[Load[Expr]] with
        override val decoder: AbstractDecoder = Load.this.getDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Load[Expr] =
            for (key, value) <- loadInfo do value.load(reader.readMember(key)(using value.decoder, decoder).value)
            return Load.this

    override def load(filename: String): Unit =
        val bytes = Files.readAllBytes(Paths.get(filename))
        if bytes != null then Json.decode(bytes).to[Load[Expr]].value

    def loadInfo: Map[String, Loadable[_]] =
        Map("name" -> Loadable((name: String) => println(name)))

trait LoadModF
    extends Load[SchemeExp]
    with LoadComponents[SchemeExp]
    with LoadStandardSchemeComponents
    with LoadNoContext[SchemeExp]
    with LoadSchemeAddr
    with LoadDependency[SchemeExp]
    with LoadAddrDependency[SchemeExp]
    with LoadStandardSchemeComponentID
    with LoadGlobalStore[SchemeExp]
    with LoadModularSchemeLattices:
    def getDecoder: AbstractDecoder = new MapDecoder
    def getKeyDecoder: AbstractDecoder = new MapDecoder
