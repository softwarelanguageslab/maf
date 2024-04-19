package maf.save

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
import io.bullet.borer.derivation.CompactMapBasedCodecs

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
    override protected given expressionSetDecoder: MapDecoder[Set[Expr]] with
        override def read(reader: Reader): Set[Expr] =
            reader.start()
            val expressions = reader.readUntilBeforeBreak(
              Set[Expr](),
              (expressions: Set[Expr]) =>
                  val expression = reader.readMember[Expr]()(using actualExpressionDecoder)
                  val key = expression.key.toInt
                  LoadExpressionIntID.this.expressions.addOne((key, expression.value))
                  expressions + (expression.value)
            )
            reader.close()
            return expressions
    override protected given expressionIDDecoder: Decoder[Expr] with
        override def read(reader: Reader): Expr =
            if reader.hasInt then return expressions(reader.readInt())
            else reader.read[Expr]()(using actualExpressionDecoder)

trait LoadSchemeSubExpressions extends LoadExpressions[SchemeExp] with LoadPosition[SchemeExp]:
    given Decoder[SchemeValue] = CompactMapBasedCodecs.deriveDecoder
    given Decoder[maf.language.sexp.Value] = CompactMapBasedCodecs.deriveAllDecoders
    given Decoder[SchemeFuncall] = CompactMapBasedCodecs.deriveDecoder[SchemeFuncall]
    given Decoder[SchemeVar] = CompactMapBasedCodecs.deriveDecoder[SchemeVar]
    given Decoder[SchemeLambda] = CompactMapBasedCodecs.deriveDecoder[SchemeLambda]
    given Decoder[SchemeVarArgLambda] = CompactMapBasedCodecs.deriveDecoder[SchemeVarArgLambda]
    given Decoder[SchemeLambdaExp] = CompactMapBasedCodecs.deriveDecoder[SchemeLambdaExp]
    given Decoder[SchemeLetrec] = CompactMapBasedCodecs.deriveDecoder[SchemeLetrec]
    given Decoder[SchemeAssert] = CompactMapBasedCodecs.deriveDecoder[SchemeAssert]
    given Decoder[SchemeLet] = CompactMapBasedCodecs.deriveDecoder[SchemeLet]
    given Decoder[SchemeIf] = CompactMapBasedCodecs.deriveDecoder[SchemeIf]
    given Decoder[SchemeSet] = CompactMapBasedCodecs.deriveDecoder[SchemeSet]
    given Decoder[SchemeBegin] = CompactMapBasedCodecs.deriveDecoder[SchemeBegin]
    given Decoder[SchemeLetStar] = CompactMapBasedCodecs.deriveDecoder[SchemeLetStar]

trait LoadSchemeExpressions extends LoadActualExprs[SchemeExp] with LoadSchemeSubExpressions:
    override protected given actualExpressionDecoder: MapDecoder[SchemeExp] with
        override def read(reader: Reader): SchemeExp =
            reader.start()
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
            reader.close()
            return expression.value
