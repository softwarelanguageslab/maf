package maf.language.ContractScheme.interpreter

import maf.language.sexp.*
import maf.util.Reader
import maf.core.Identity
import scala.util.Try
import maf.util.ArrayEq
import maf.core.Monad
import maf.language.ContractScheme.ContractValues

object RandomInputsFromFile:
    def toInputPath(sourcePath: String): String =
        s"input/generated/${sourcePath.replace("/", "_")}.scm"

/**
 * MAF can read random inputs that serve as the source for provide/contract-out forms from an input file
 *
 * The input file should be of the following form: each line should contain an s-expression with the following format: (name INPUT-1 INPUT-2 ...)
 *
 * such that "name" is the name of the identifier as it appears in the original provide/contract-out form and INPUT-1, INPUT-2 are concrete inputs.
 * The 'fail symbol can be used to indicate that no random input exists that satisfies this contract, and that a purely random value should be used.
 *
 * This symbol may reside on both the INPUT-1, INPUT-2, ... or instead of the s-expression on a seperate line.
 *
 * @param sourcePath
 *   the path to the input file
 */
class RandomInputsFromFile(sourcePath: String) extends RandomInputGenerator:
    /**
     * Reads and parses the source file from the given `sourcePath` into a map, mapping identifier names to lists of values (for their arguments);
     *
     * another pass is required to obtain concrete values from these value literals
     */
    def loadFile(): Map[String, List[SExp]] =
        import SExpUtils.*
        val contents = Reader.loadFile(sourcePath)
        val parsed = SExpParser.parse(contents)
        parsed.foldLeft(Map[String, List[SExp]]())((resultMap, exp) =>
            resultMap + (exp match
                case Ident(name) :::: values => name -> values.toList
                case Ident("fail")           => "fail" -> List()
                case _                       => throw new Exception(s"could not load $sourcePath as a source of random inputs, incorrect format")
            )
        )

    import SExpUtils.*

    def convertToConcreteValue(quoted: Boolean)(sexp: SExp): InputGenerator =
        import maf.core.Monad.MonadSyntaxOps
        sexp match
            case SExpValue(vlu, _) =>
                noalloc(vlu match
                    case Value.String(v)    => ConcreteValues.Value.Str(v)
                    case Value.Symbol(s)    => ConcreteValues.Value.Symbol(s)
                    case Value.Integer(v)   => ConcreteValues.Value.Integer(v)
                    case Value.Real(v)      => ConcreteValues.Value.Real(v)
                    case Value.Boolean(v)   => ConcreteValues.Value.Bool(v)
                    case Value.Character(v) => ConcreteValues.Value.Character(v)
                    case Value.Nil          => ConcreteValues.Value.Nil
                )
            case Ident("+inf.0") => noalloc(ConcreteValues.Value.Real(Double.PositiveInfinity))
            case Ident("-inf.0") => noalloc(ConcreteValues.Value.Real(Double.NegativeInfinity))

            // A quote is recursively converted
            case Ident("quote") :::: contents => convertToConcreteValue(quoted = true)(contents)
            // What follows is how to convert a quoted expression to concrete pairs
            case car :::: cdr if quoted =>
                for
                    mcar <- convertToConcreteValue(quoted)(car)
                    mcdr <- convertToConcreteValue(quoted)(cdr)
                    mcons <- alloc(ConcreteValues.Value.Cons(mcar, mcdr))
                yield mcons

            case SNil(_) if quoted =>
                noalloc(ConcreteValues.Value.Nil)

            case Ident("fail") =>
                throw new Exception("could not generate value")

            case SExpVector(Ident(head) :: elements, _) if head.startsWith("struct:") =>
                // for now vectors only enable the definition of structs.
                for elementValues <- Monad.sequence(elements.map(convertToConcreteValue(false)))
                yield ConcreteValues.ContractValue(ContractValues.Struct(head.replace("struct:", "").nn, ArrayEq.from(elementValues)))

            // if not quoted
            case l @ (_ :::: _) => convertToConcreteValue(quoted = true)(l)
            case Ident(sym)     => noalloc(ConcreteValues.Value.Symbol(sym))

            // all other expression types are not supported as value literals
            case _ => throw new Exception(s"unsupported expression $sexp as concrete input ${if quoted then "in a quoted environment" else ""}")

    /** Convert the given map of value literal to a map of concrete values */
    def convertMapToConcreteValue(input: Map[String, List[SExp]]): Map[String, List[InputGenerator]] =
        input.view
            .mapValues(vlus => Try(vlus map convertToConcreteValue(quoted = false)).getOrElse(List()))
            .toMap

    private lazy val inputs: Map[String, List[InputGenerator]] =
        convertMapToConcreteValue(loadFile())

    /**
     * For the given contracts and topLevel function (if available) generate a concrete input from the file.
     *
     * If the file does not contain any concrete input, then randomly generated a concrete input
     */
    override def generateInput(topLevelFunction: String, contract: Set[String] = Set()): List[InputGenerator] =
        // ignore the set of contracts, lets hope that Racket was able to generate some useful values
        inputs.get(topLevelFunction).getOrElse(List())
