package maf.save

import maf.save.EncapsulatedDecoder.*

import maf.core.Expression
import io.bullet.borer.Decoder
import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeValue
import maf.language.scheme.SchemeFuncall
import maf.language.scheme.SchemeVar
import maf.language.scheme.SchemeLambda
import maf.language.scheme.SchemeVarArgLambda
import maf.language.scheme.SchemeLambdaExp
import maf.language.scheme.SchemeLetrec
import maf.language.scheme.SchemeLet
import maf.language.scheme.SchemeIf
import maf.language.scheme.SchemeSet
import maf.language.scheme.SchemeBegin
import maf.language.scheme.SchemeLetStar
import maf.language.scheme.SchemeAssert
import io.bullet.borer.Reader
import scala.collection.mutable.HashMap

/**
 * The base trait for decoding expressions.
 *
 * @note
 *   This trait gives the methods needed to decode expressions, but does not implement them yet, other traits like [[LoadSchemeExpressions]] or
 *   [[LoadExpressionIntID]] should be mixed in for the implementation. The trait that should be mixed in depends on the kind of expressions are used
 *   in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadExpressions[Expr <: Expression] extends Load[Expr]:
    /**
     * Get the decoder that will be used to decode expressions.
     *
     * This will influence how expressions will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getExpressionDecoder: AbstractDecoder = getDecoder

    /**
     * Get the decoder that will be used to decode expressions.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how expressions will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getExpressionKeyDecoder: AbstractDecoder = getKeyDecoder

    given expressionDecoder: Decoder[Expr]

/**
 * The base trait for decoding expressions.
 *
 * This trait is used to add [[actualExpressionDecoder]], this given cannot be added into [[LoadExpressions]] because this would cause an ambigious
 * implicit with [[expressionEncoder]].
 *
 * @note
 *   This trait gives the methods needed to decode expressions, but does not implement them yet, other traits like [[LoadSchemeExpressions]] or
 *   [[LoadExpressionIntID]] should be mixed in for the implementation. The trait that should be mixed in depends on the kind of expressions are used
 *   in your analysis.
 * @note
 *   This trait should not be used, rather, [[LoadExpressions]] should be extended.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadActualExprs[Expr <: Expression] extends LoadExpressions[Expr]:
    /** Decodes the actual expression, and doesn't decode it using IDs. */
    protected given actualExpressionDecoder: Decoder[Expr]

trait LoadActualExpressions[Expr <: Expression] extends LoadActualExprs[Expr]:
    override given expressionDecoder: Decoder[Expr] = actualExpressionDecoder

trait LoadExpressionID[Expr <: Expression] extends Load[Expr] with LoadActualExprs[Expr]:
    override given expressionDecoder: Decoder[Expr] = expressionIDDecoder
    override def loadInfo: List[(String, Loadable[?])] =
        super.loadInfo ++ List(("expressions", Loadable((expressions: Set[Expr]) => ())))

    /** Decodes a set of expressions, this is used to e.g. get ID info to the expressions. */
    protected given expressionSetDecoder: Decoder[Set[Expr]]

    /** Decodes an expression using an ID */
    protected given expressionIDDecoder: Decoder[Expr]

trait LoadExpressionIntID[Expr <: Expression] extends LoadExpressionID[Expr]:
    private val expressions: HashMap[Int, Expr] = HashMap[Int, Expr]()
    override protected given expressionSetDecoder: EncapsulatedDecoder[Set[Expr]] with
        override def decoder: AbstractDecoder = getExpressionKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using decoder: AbstractDecoder): Set[Expr] =
            return reader.readUntilBeforeBreak(
              Set[Expr](),
              (expressions: Set[Expr]) =>
                  val expression = reader.readMember[Expr]()(using actualExpressionDecoder, decoder)
                  val key = expression.key.toInt
                  LoadExpressionIntID.this.expressions.addOne((key, expression.value))
                  expressions + (expression.value)
            )
    override protected given expressionIDDecoder: Decoder[Expr] with
        override def read(reader: Reader): Expr =
            if reader.hasInt then return expressions(reader.readInt())
            else reader.read[Expr]()(using actualExpressionDecoder)

trait LoadSchemeSubExpressions extends LoadExpressions[SchemeExp] with LoadPosition[SchemeExp]:
    private val compDecoder = getExpressionDecoder
    given Decoder[SchemeValue] = AbstractDecoder.deriveDecoder(compDecoder)
    given Decoder[maf.language.sexp.Value] = AbstractDecoder.deriveAllDecoders(compDecoder)
    given Decoder[SchemeFuncall] = AbstractDecoder.deriveDecoder[SchemeFuncall](compDecoder)
    given Decoder[SchemeVar] = AbstractDecoder.deriveDecoder[SchemeVar](compDecoder)
    given Decoder[SchemeLambda] = AbstractDecoder.deriveDecoder[SchemeLambda](compDecoder)
    given Decoder[SchemeVarArgLambda] = AbstractDecoder.deriveDecoder[SchemeVarArgLambda](compDecoder)
    given Decoder[SchemeLambdaExp] = AbstractDecoder.deriveDecoder[SchemeLambdaExp](compDecoder)
    given Decoder[SchemeLetrec] = AbstractDecoder.deriveDecoder[SchemeLetrec](compDecoder)
    given Decoder[SchemeAssert] = AbstractDecoder.deriveDecoder[SchemeAssert](compDecoder)
    given Decoder[SchemeLet] = AbstractDecoder.deriveDecoder[SchemeLet](compDecoder)
    given Decoder[SchemeIf] = AbstractDecoder.deriveDecoder[SchemeIf](compDecoder)
    given Decoder[SchemeSet] = AbstractDecoder.deriveDecoder[SchemeSet](compDecoder)
    given Decoder[SchemeBegin] = AbstractDecoder.deriveDecoder[SchemeBegin](compDecoder)
    given Decoder[SchemeLetStar] = AbstractDecoder.deriveDecoder[SchemeLetStar](compDecoder)

trait LoadSchemeExpressions extends LoadActualExprs[SchemeExp] with LoadSchemeSubExpressions:
    override protected given actualExpressionDecoder: EncapsulatedDecoder[SchemeExp] with
        override val decoder = getExpressionKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): SchemeExp =
            val expression = reader.readMembers[SchemeExp](
              Array(
                ("funcall", summon[Decoder[SchemeFuncall]]),
                ("var", summon[Decoder[SchemeVar]]),
                ("lambda", summon[Decoder[SchemeLambda]]),
                ("argLambda", summon[Decoder[SchemeVarArgLambda]]),
                ("value", summon[Decoder[SchemeValue]]),
                ("letrec", summon[Decoder[SchemeLetrec]]),
                ("assert", summon[Decoder[SchemeAssert]]),
                ("let", summon[Decoder[SchemeLet]]),
                ("schemeIf", summon[Decoder[SchemeIf]]),
                ("set", summon[Decoder[SchemeSet]]),
                ("begin", summon[Decoder[SchemeBegin]]),
                ("letStar", summon[Decoder[SchemeLetStar]]),
              )
            )
            expression.value
