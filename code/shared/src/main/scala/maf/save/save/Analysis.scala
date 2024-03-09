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
import maf.save.save.SaveSchemeExpressions
import maf.save.save.SaveRecursiveSchemeExpressionsIntID

/**
 * Contains info about the top-level objects that need to be saved.
 *
 * @param value
 *   The value that needs to be saved
 * @param encoder
 *   Encodes the value
 * @tparam T
 *   The type of the value the needs to be saved
 */
case class Savable[T](val value: T)(using val encoder: Encoder[T])

/**
 * The base trait for saving an analysis.
 *
 * Implementing this allows you to save your analysis, by default it will only save the name of your analysis and you should mixin other traits like
 * [[SaveComponents]] to also save components.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait Save[Expr <: Expression] extends ModAnalysis[Expr]:
    /**
     * Get the encoder that will be used to encode your analysis.
     *
     * This will influence how the analysis will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getEncoder: AbstractEncoder

    /**
     * Get the encoder that will be used to encode your analysis.
     *
     * This encoder is used to encode objects where the key is important, when you e.g. encode a type in the key, some encoders might remove this key,
     * and should therefore not be used here.
     *
     * This will influence how the analysis will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getKeyEncoder: AbstractEncoder

    /** Encode an analysis. */
    given EncapsulatedEncoder[Save[Expr]] with
        override val encoder = Save.this.getEncoder
        override def writeEncapsulated(writer: Writer, value: Save[Expr]): Writer =
            for (key, value) <- saveInfo do writer.writeMember(key, value.value)(using value.encoder, encoder)
            writer

    override def save(filename: String): Unit =
        val res = Json.encode(this).toByteArray
        Files.write(Paths.get(filename), res)

    /**
     * Returns a map strings and [[Savable]] s.
     *
     * This map defines all top-level objects that should be saved in your analysis, and the key with which they should be saved. If you want to save
     * something else, you can override this method and add something to it.
     *
     * {{{
     * override def saveInfo: Map[String, Savable[_]] =
     *     super.saveInfo + ("< key >" -> Savable(< saveValue >))
     * }}}
     */
    def saveInfo: List[(String, Savable[_])] = List(("name", Savable(analysisName)))

/** The trait used to save the modF analysis. */
trait SaveModF
    extends Save[SchemeExp]
    with SaveSchemeExpressions
    with SaveRecursiveSchemeExpressionsIntID
    with SaveComponentIntID[SchemeExp]
    with SaveStandardSchemeComponents
    with SaveModularDomain
    with SaveAddrDep[SchemeExp]
    with SaveSchemeAddr
    with SaveGlobalStore[SchemeExp]
    with SaveModularSchemeLattices
    with SaveNoContext[SchemeExp]:
    override def getEncoder: AbstractEncoder = new MapEncoder
    override def getKeyEncoder: AbstractEncoder = new MapEncoder
