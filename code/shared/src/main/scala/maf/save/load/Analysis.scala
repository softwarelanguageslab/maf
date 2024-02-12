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

/**
 * Contains info about the top-level objects that need to be loaded.
 *
 * @param load
 *   Function that should be called with the value after it was decoded
 * @param decoder
 *   Decodes the value
 * @tparam T
 *   The type of the value the needs to be loaded
 */
case class Loadable[T](val load: (T) => Unit)(using val decoder: Decoder[T])

/**
 * The base trait for saving an analysis.
 *
 * Implementing this allows you to load your analysis, by default it will only load the name of your analysis and you should mixin other traits like
 * [[LoadComponents]] to also load components.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait Load[Expr <: Expression] extends ModAnalysis[Expr]:
    /**
     * Get the decoder that will be used to decode your analysis.
     *
     * This will influence how the analysis will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getDecoder: AbstractDecoder

    /**
     * Get the decoder that will be used to decode your analysis.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how the analysis will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getKeyDecoder: AbstractDecoder

    /** Decode an analysis. */
    given EncapsulatedDecoder[Load[Expr]] with
        override val decoder: AbstractDecoder = Load.this.getDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Load[Expr] =
            for (key, value) <- loadInfo do value.load(reader.readMember(key)(using value.decoder, decoder).value)
            return Load.this

    override def load(filename: String): Unit =
        val bytes = Files.readAllBytes(Paths.get(filename))
        if bytes != null then Json.decode(bytes).to[Load[Expr]].value

    /**
     * Returns a map strings and [[Loadable]] s.
     *
     * This map defines all top-level objects that should be loaded in your analysis, and the key with which they should be loaded. If you want to
     * load something else, you can override this method and add something to it.
     *
     * {{{
     * override def loadInfo: Map[String, Loadable[_]] =
     *     super.loadInfo + ("< key >" -> Loadable[< loadType >]((< loaded object >: < SaveType >) => < put loaded object into analysis >))
     * }}}
     */
    def loadInfo: Map[String, Loadable[_]] =
        Map("name" -> Loadable((name: String) => println(name)))

/** The trait used to load the modF analysis. */
trait LoadModF
    extends Load[SchemeExp]
    with LoadComponents[SchemeExp]
    with LoadStandardSchemeComponents
    with LoadNoContext[SchemeExp]
    with LoadSchemeAddr
    with LoadDependency[SchemeExp]
    with LoadAddrDependency[SchemeExp]
    with LoadStandardSchemeComponentPosition
    with LoadGlobalStore[SchemeExp]
    with LoadModularSchemeLattices:
    def getDecoder: AbstractDecoder = new MapDecoder
    def getKeyDecoder: AbstractDecoder = new MapDecoder
