package maf.save.save

import maf.core.Expression
import maf.save.Save
import maf.save.AbstractEncoder
import io.bullet.borer.Encoder
import maf.save.Savable
import scala.collection.mutable.HashMap
import maf.save.EncapsulatedEncoder
import io.bullet.borer.Writer
import EncapsulatedEncoder.*
import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeFuncall
import maf.language.scheme.SchemeLambda
import maf.language.scheme.SchemeVarArgLambda
import maf.language.scheme.SchemeLetrec
import maf.language.scheme.SchemeAssert
import maf.language.scheme.SchemeLet
import maf.language.scheme.SchemeIf
import maf.language.scheme.SchemeSet
import maf.language.scheme.SchemeBegin
import maf.language.scheme.SchemeLetStar
import maf.save.SavePosition
import maf.save.ArrayEncoder
import maf.language.scheme.SchemeVar
import maf.language.scheme.SchemeValue
import maf.language.scheme.SchemeLambdaExp
import maf.modular.ModAnalysis

/**
 * The base trait for encoding expressions.
 *
 * @note
 *   This trait gives the methods needed to encode expressions, but does not implement them yet, other traits like [[SaveSchemeExpressions]] or
 *   [[SaveExpressionIntID]] should be mixed in for the implementation. The trait that should be mixed in depends on the kind of components are used
 *   in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveExpressions[Expr <: Expression] extends Save[Expr]:
    /**
     * Get the encoder that will be used to encode expressions.
     *
     * This will influence how context will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getExpressionEncoder: AbstractEncoder = getEncoder

    /**
     * Get the encoder that will be used to encode expression.
     *
     * This encoder is used to encode objects where the key is important, when you e.g. encode a type in the key, some encoders might remove this key,
     * and should therefore not be used here.
     *
     * This will influence how components will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getExpressionKeyEncoder: AbstractEncoder = getKeyEncoder

    /** Encodes an expression */
    given expressionEncoder: Encoder[Expr]

/**
 * The base trait for encoding expressions.
 *
 * This trait is used to add [[actualExpressionEncoder]], this given cannot be added into [[SaveExpressions]] because this would cause an ambigious
 * implicit with [[expressionEncoder]].
 *
 * @note
 *   This trait gives the methods needed to encode expressions, but does not implement them yet, other traits like [[SaveSchemeExpressions]] or
 *   [[SaveExpressionIntID]] should be mixed in for the implementation. The trait that should be mixed in depends on the kind of components are used
 *   in your analysis.
 * @note
 *   This trait should not be used, rather, [[SaveExpressions]] should be extended.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveActualExprs[Expr <: Expression] extends SaveExpressions[Expr]:
    /** Encodes the actual expression, and doesn't encode it using IDs. */
    protected given actualExpressionEncoder: Encoder[Expr]

/**
 * The base trait for encoding expressions as IDs.
 *
 * This is an implementation of [[SaveExpressions]]
 *
 * @note
 *   This trait gives the methods needed to encode expression IDs, but does not implement them yet, other traits like
 *   [[SaveRecursiveSchemeExpressionsIntID]] or [[SaveExpressionIntID]] should be mixed in for the implementation. The trait that should be mixed in
 *   depends on the kind of components are used in your analysis.
 * @note
 *   This trait will first save all necessary expressions separately and use IDs to save them to following times.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveExpressionID[Expr <: Expression] extends ModAnalysis[Expr] with Save[Expr] with SaveActualExprs[Expr]:
    override given expressionEncoder: Encoder[Expr] = expressionIDEncoder
    override def saveInfo: List[(String, Savable[_])] = super.saveInfo ++ List(("expressions" -> Savable(visited.map(expr(_)))))

    /** Encodes a set of expressions, this is used to e.g. add ID info to the expressions. */
    protected given expressionSetEncoder: Encoder[Set[Expr]]

    /** Encodes an expression using an ID */
    protected given expressionIDEncoder: Encoder[Expr]

/**
 * Trait to encode expressions using integer IDs.
 *
 * Implementation of [[SaveExpressionID]]
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveExpressionIntID[Expr <: Expression] extends SaveExpressionID[Expr] with SaveExpressions[Expr]:
    private val expressions = HashMap[Expr, Int]()
    private var id = 0

    override protected given expressionSetEncoder: EncapsulatedEncoder[Set[Expr]] with
        override val encoder = getExpressionKeyEncoder
        def writeEncapsulated(writer: Writer, exprs: Set[Expr]): Writer =
            for (expr <- exprs) do
                println(id)
                writer.writeMember(id.toString, expr)(using actualExpressionEncoder, encoder)
                expressions.addOne((expr, id))
                id += 1
            writer

    override protected given expressionIDEncoder: Encoder[Expr] with
        override def write(writer: Writer, expr: Expr): Writer =
            if expressions.contains(expr) then writer.write(expressions(expr))
            else writer.write(expr)(using actualExpressionEncoder)

/**
 * Trait to encode scheme expressions recursively using integer IDs.
 *
 * This will recursively save every expression, this means that if you are e.g. encoding an `if` statement, this will first encode the condition, the
 * consequence and the alternative, and only then save the actual `if` statement.
 *
 * Implementation of [[SaveExpressionID]]
 */
trait SaveRecursiveSchemeExpressionsIntID extends SaveExpressionID[SchemeExp] with SaveExpressions[SchemeExp]:
    private val expressions = HashMap[SchemeExp, Int]()
    private var id = 0

    override protected given expressionSetEncoder: EncapsulatedEncoder[Set[SchemeExp]] with
        override val encoder = getExpressionKeyEncoder

        given Encoder[List[SchemeExp]] with
            override def write(writer: Writer, exprs: List[SchemeExp]): Writer =
                for (expr <- exprs) writer.write(expr)(using recursiveExpressionEncoder)
                writer

        given recursiveExpressionEncoder: Encoder[SchemeExp] with
            override def write(writer: Writer, expr: SchemeExp): Writer =
                if expressions.contains(expr) then return writer
                expr match
                    case funcall: SchemeFuncall =>
                        writer.write(funcall.args)
                        writer.write(funcall.f)(using recursiveExpressionEncoder)
                    case lambda: SchemeLambda          => writer.write(lambda.body)
                    case argLambda: SchemeVarArgLambda => writer.write(argLambda.body)
                    case letrec: SchemeLetrec =>
                        for (binding <- letrec.bindings) writer.write(binding._2)
                        writer.write(letrec.body)
                    case assert: SchemeAssert => writer.write(assert.exp)
                    case let: SchemeLet =>
                        for (binding <- let.bindings) writer.write(binding._2)
                        writer.write(let.body)
                    case schemeIf: SchemeIf =>
                        writer.write(schemeIf.cond)
                        writer.write(schemeIf.cons)
                        writer.write(schemeIf.alt)
                    case set: SchemeSet =>
                        writer.write(set.value)
                    case begin: SchemeBegin =>
                        writer.write(begin.exps)
                    case letStar: SchemeLetStar =>
                        for (binding <- letStar.bindings) writer.write(binding._2)
                        writer.write(letStar.body)
                    case _ => writer

                writer.writeMember(id.toString(), expr)(using actualExpressionEncoder, encoder)
                expressions.addOne(expr, id)
                id += 1
                writer

        def writeEncapsulated(writer: Writer, exprs: Set[SchemeExp]): Writer =
            for (expr <- exprs) do writer.write(expr)(using recursiveExpressionEncoder)
            writer

    override protected given expressionIDEncoder: Encoder[SchemeExp] with
        override def write(writer: Writer, expr: SchemeExp): Writer =
            if expressions.contains(expr) then writer.write(expressions(expr))
            else writer.write(expr)(using actualExpressionEncoder)

/**
 * Save the expressions normally.
 *
 * Implementation of [[SaveExpressions]].
 */
trait SaveActualExpressions[Expr <: Expression] extends SaveActualExprs[Expr]:
    override given expressionEncoder: Encoder[Expr] = actualExpressionEncoder

/**
 * Save Scheme expressions.
 *
 * Implementation of [[SaveExpressions]].
 */
trait SaveSchemeExpressions extends SaveActualExprs[SchemeExp] with SaveSchemeSubExpressions with SavePosition[SchemeExp]:
    override protected given actualExpressionEncoder: EncapsulatedEncoder[SchemeExp] with
        override val encoder = getExpressionKeyEncoder
        def writeEncapsulated(writer: Writer, exp: SchemeExp): Writer =
            exp match
                case funcall: SchemeFuncall        => writer.writeMember("funcall", funcall)
                case variable: SchemeVar           => writer.writeMember("var", variable)
                case lambda: SchemeLambda          => writer.writeMember("lambda", lambda)
                case argLambda: SchemeVarArgLambda => writer.writeMember("argLambda", argLambda)
                case value: SchemeValue            => writer.writeMember("value", value)
                case letrec: SchemeLetrec          => writer.writeMember("letrec", letrec)
                case assert: SchemeAssert          => writer.writeMember("assert", assert)
                case let: SchemeLet                => writer.writeMember("let", let)
                case schemeIf: SchemeIf            => writer.writeMember("schemeIf", schemeIf)
                case set: SchemeSet                => writer.writeMember("set", set)
                case begin: SchemeBegin            => writer.writeMember("begin", begin)
                case letStar: SchemeLetStar        => writer.writeMember("letStar", letStar)
                case _ =>
                    System.err.nn.println("The scheme expression with type `" + exp.getClass + "` could not be encoded")
                    writer

/**
 * Save Scheme subexpressions.
 *
 * Implementation of [[SaveExpressions]].
 */
trait SaveSchemeSubExpressions extends SaveExpressions[SchemeExp] with SavePosition[SchemeExp]:
    private val compEncoder = getExpressionEncoder
    given Encoder[SchemeValue] = AbstractEncoder.deriveEncoder(compEncoder)
    given Encoder[maf.language.sexp.Value] = AbstractEncoder.deriveAllEncoders(compEncoder)
    given Encoder[SchemeFuncall] = AbstractEncoder.deriveEncoder[SchemeFuncall](compEncoder)
    given Encoder[SchemeVar] = AbstractEncoder.deriveEncoder[SchemeVar](compEncoder)
    given Encoder[SchemeLambda] = AbstractEncoder.deriveEncoder[SchemeLambda](compEncoder)
    given Encoder[SchemeVarArgLambda] = AbstractEncoder.deriveEncoder[SchemeVarArgLambda](compEncoder)
    given Encoder[SchemeLambdaExp] = AbstractEncoder.deriveEncoder[SchemeLambdaExp](compEncoder)
    given Encoder[SchemeLetrec] = AbstractEncoder.deriveEncoder[SchemeLetrec](compEncoder)
    given Encoder[SchemeAssert] = AbstractEncoder.deriveEncoder[SchemeAssert](compEncoder)
    given Encoder[SchemeLet] = AbstractEncoder.deriveEncoder[SchemeLet](compEncoder)
    given Encoder[SchemeIf] = AbstractEncoder.deriveEncoder[SchemeIf](compEncoder)
    given Encoder[SchemeSet] = AbstractEncoder.deriveEncoder[SchemeSet](compEncoder)
    given Encoder[SchemeBegin] = AbstractEncoder.deriveEncoder[SchemeBegin](compEncoder)
    given Encoder[SchemeLetStar] = AbstractEncoder.deriveEncoder[SchemeLetStar](compEncoder)
