package maf.cli.modular.scv

import maf.modular.scv.{IsSat, Sat, ScvReporter, ScvSatSolver, Unknown, Unsat}
import maf.language.symbolic.*
import maf.core.Address
import maf.language.scheme._
import maf.language.sexp.Value
import maf.language.scheme.lattices.SchemeLattice
import maf.core.Identifier
import smtlib.parser.Parser
import smtlib.trees.Commands._
import smtlib.Interpreter
import maf.language.symbolic.Symbolic
import smtlib.trees.CommandsResponses.{CheckSatStatus, SatStatus, UnsatStatus}
import smtlib.interpreters.{Z3Interpreter, Z3InterpreterNative}
import scala.concurrent.ExecutionContext

class JVMSatSolver[V](reporter: ScvReporter)(using SchemeLattice[V, Address]) extends ScvSatSolver[V]:
    /** We run the Z3 process in the background, and feed it with commands */
    private given z3Interpreter: Interpreter = Z3Interpreter.buildDefault

    /** Sends the reset command to the Z3 interpreter */
    private def reset()(using interpreter: Interpreter, ec: ExecutionContext): Unit =
        // We pop to the initial state of the solver
        interpreter.eval(Pop(1))
        // And mark this point again
        interpreter.eval(Push(1))

    /** A cache where already solved path conditions are stored together with their result */
    private var cache: Map[(Formula, List[String]), IsSat[V]] = Map()

    /**
     * Checks whether the path condition is already solved
     *
     * @param e
     *   the expression representing the path condition (as a list that represents conjunctions of expressions)
     * @param vars
     *   the variables used in the path condition
     */
    def inCache(e: Formula, vars: List[String]): Boolean = cache.contains((e, vars))

    /** Returns the cached version of the path condition */
    private def lookupCache(e: Formula, vars: List[String]): IsSat[V] = cache((e, vars))

    /** Stores the answer in the cache */
    private def storeCache(e: Formula, vars: List[String], v: IsSat[V]): Unit =
        cache = (cache + ((e, vars) -> v))

    /** A mapping between the name of Scheme primitives and their Z3 counter-parts */
    private val primMap: Map[String, String] = Map(
      "number?" -> "number?/v",
      "boolean?" -> "boolean?/v",
      "string?" -> "string?/v",
      "true?" -> "true?/v",
      "false?" -> "false?/v",
      "null?" -> "null?/v",
      "real?" -> "real?/v",
      "integer?" -> "integer?/v",
      "any?" -> "any?/v",
      "conj/v" -> "and",
      "disj/v" -> "or",
      "not" -> "not/v",
      "-" -> "-/v",
      "+" -> "+/v",
      "*" -> "*/v",
      "/" -> "//v",
      ">" -> ">/v",
      "<" -> "</v",
      "<=" -> "<=/v",
      ">=" -> ">=/v"
    )

    /** Translates a symbolic Scheme value to an instance of the `V` sort */
    private def injectValue(value: Value): String = value match
        case Value.String(value)   => s"(VString $value)"
        case Value.Symbol(symbol)  => s"(VSymbol $symbol)"
        case Value.Integer(value)  => s"(VInteger $value)"
        case Value.Real(value)     => s"(VReal $value)"
        case Value.Boolean(b) if b => s"(VBool true)"
        case Value.Boolean(b)      => s"(VBool false)"
        case Value.Character(c)    => throw new Exception(s"Not supported character ($c)") // TODO
        case Value.Nil             => s"VNil"

    private def translateIdentifier(idn: Identifier): String =
        primMap.get(idn.name).getOrElse(idn.name)

    /** A SMTLIB2 program that will be prepended to the actual constraints generated b our analyses */
    private val prelude: String = """
     | ;; represent the Scheme/Racket types as Z3 types 
     | ;; TODO: check how cons, vector, ptr, and procedures can be represented, maybe we will need some representation of the heap?
     |  (declare-datatypes ()
     |    ((V (VInteger (unwrap-VInteger Int))
     |        (VReal    (unwrap-VReal    Real))
     |        (VNil)
     |        (VBool    (unwrap-bool    Bool))
     |        (VString  (unwrap-string  String))
     |        (VSymbol  (unwrap-symbol String))
     |        (VError))))
     |
     |  (define-fun boolean?/v ((b V)) V
     |     (VBool (is-VBool b)))
     |
     |  (define-fun null?/v ((n V)) V
     |     (VBool (is-VNil n)))
     |
     |  (declare-fun fresh () V)
     |
     |  (define-fun string?/v ((s V)) V
     |     (VBool (is-VString s)))
     |
     |  (define-fun integer?/v ((n V)) V
     |     (VBool (is-VInteger n)))
     |
     |  (define-fun number?/v ((n V)) V 
     |    (VBool (or (is-VReal n) (is-VInteger n))))
     |
     |  (define-fun real?/v ((n V)) V
     |     (VBool (is-VReal n)))
     | 
     |  (define-fun any?/v ((n V)) V 
     |      (VBool true))
      | (declare-fun string-length (V) V) 
      | 
      | (define-fun +/v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VInteger (+ (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VReal (+ (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VReal (+ (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VReal (+ (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      |   
      | (define-fun -/v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VInteger (- (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VReal (- (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VReal (- (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VReal (- (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      |   
      | (define-fun */v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VInteger (* (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VReal (* (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VReal (* (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VReal (* (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      |   
      | (define-fun //v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VInteger (/ (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VReal (/ (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VReal (/ (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VReal (/ (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      |   (define-fun =/v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VBool (= (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VBool (= (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VBool (= (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VBool (= (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      |   
      | (define-fun </v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VBool (< (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VBool (< (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VBool (< (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VBool (< (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      |   
      | (define-fun >/v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VBool (> (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VBool (> (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VBool (> (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VBool (> (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      |   
      | (define-fun <=/v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VBool (<= (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VBool (<= (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VBool (<= (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VBool (<= (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      |   
      | (define-fun >=/v ((a V) (b V)) V
      |   (ite (and (is-VInteger a) (is-VInteger b)) 
      |        (VBool (>= (unwrap-VInteger a) (unwrap-VInteger b)))
      |        (ite (and (is-VReal a) (is-VInteger b))
      |             (VBool (>= (unwrap-VReal a) (unwrap-VInteger b)))
      |             (ite (and (is-VInteger a) (is-VReal b))
      |                  (VBool (>= (unwrap-VInteger a) (unwrap-VReal b)))
      |                  (ite (and (is-VReal a) (is-VReal b))
      |                       (VBool (>= (unwrap-VReal a) (unwrap-VReal b)))
      |                       VError)))))
      | 
     |
     | (define-fun not/v ((a V)) V
     |   (ite (is-VBool a) (VBool (not (unwrap-VBool a))) VError))
     |
     |  (define-fun true?/v ((n V)) Bool
     |     (ite (is-VBool n) (unwrap-bool n) false))
     |  (define-fun false?/v ((b V)) Bool
     |     (ite (is-VBool b) (not (unwrap-bool b)) false))
     | (push 1)
    """.stripMargin

    /** We pre-parse the prelude into a script */
    private lazy val parsedPrelude = parseStringToScript(prelude)

    /** Boolean flag that is set when the solver has been initialized */
    private var isInitiliazed = false

    /** Translate the given SchemeExp to a series of constraints */
    def translate(e: Formula): String = e match
        case Assertion(assertion)  => translateAssertion(assertion)
        case Conjunction(formulas) => s"(and ${formulas.map(translate).mkString(" ")})"
        case Disjunction(formulas) => s"(or ${formulas.map(translate).mkString(" ")})"
        case EmptyFormula          => ""

    def translateAssertion(e: SchemeExp): String = e match
        case SchemeVar(identifier)     => translateIdentifier(identifier)
        case SchemeValue(value, _)     => injectValue(value)
        case Symbolic.Hole(_)          => s"fresh"
        case SchemeFuncall(f, args, _) => s"(${translateAssertion(f)} ${args.map(translateAssertion).mkString(" ")})"
        case _                         => throw new Exception("Unsupported constraint")

    def parseStringToScript(s: String): Script =
        Parser.fromString(s).parseScript

    def isSat(script: Script)(using interpreter: Interpreter, ec: ExecutionContext): IsSat[V] =

        script.commands.foreach { cmd =>
            interpreter.eval(cmd)
        }

        interpreter.eval(CheckSat()) match
            case CheckSatStatus(SatStatus)   => Sat(Map())
            case CheckSatStatus(UnsatStatus) => Unsat
            case _                           => Unknown

    /** Returns either Sat, Unsat or Unknown depending on the answer of Z3 */
    def sat(e: Formula, vars: List[String]): IsSat[V] =
        import scala.concurrent.ExecutionContext.Implicits.global
        if !isInitiliazed then
            parsedPrelude.commands.foreach(z3Interpreter.eval)
            isInitiliazed = true

        if inCache(e, vars) then
            reporter.count(reporter.SATCacheHit)
            lookupCache(e, vars)
        else
            import scala.language.unsafeNulls

            val translated = e.splitConj.filterNot { _ == EmptyFormula }.map(translate).map(assertion => s"(assert $assertion)").mkString("\n")
            val varsDeclarations = vars.map(v => s"(declare-const $v V)").mkString("\n")
            val program = varsDeclarations ++ translated

            reset()
            val script: Script = Script(parseStringToScript(program).commands)
            val answer = reporter.time(reporter.Z3InterpreterTime) { isSat(script) }
            storeCache(e, vars, answer)
            answer
