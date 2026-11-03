package maf.test.language.scheme

import org.scalatest.propspec.AnyPropSpec
import maf.core._
import maf.language.scheme._
import maf.language.scheme.interpreter._
import maf.language.scheme.primitives._
import maf.modular.scheme._
import maf.modular.scheme.modflocal._
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.test.{PrimitiveTest, SchemeR5RSBenchmarks}
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

trait SchemeR5RSTests extends AnyPropSpec:

    def testExpr(program: String, answer: Any): Unit

    SchemeR5RSBenchmarks.bench.foreach { case (e, a) => property(s"Primitive in $e is correct.", PrimitiveTest)(testExpr(e, a)) }

/** Checks the result of each R5RS test program, as computed by a DSS analysis. */
trait SchemeR5RSAnalysisTests extends SchemeR5RSTests:

    type Analysis = SchemeModFLocal & SchemeDomain & SchemeModFLocalSensitivity

    def analysis(prg: SchemeExp): Analysis

    def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def testExpr(program: String, answer: Any): Unit =
        val a = analysis(parseProgram(program))
        val l = a.lattice

        import l.Injector._

        a.analyzeWithTimeout(Timeout.start(Duration(30, SECONDS)))
        // All R5RS tests should terminate, no matter the analysis, because they're so simple.
        assert(a.finished, s"Analysis of $program should finish within the given time bound out.")
        val result = l.join(a.results.getOrElse(a.MainComponent, Set.empty).map(_._1))
        assert(l.subsumes(result, answer), s"Primitive computation test failed on program: $program with result $result (expected $answer).")

/** Checks the result of each R5RS test program, as computed by a concrete interpreter. */
trait SchemeR5RSInterpreterTests extends SchemeR5RSTests:

    def interpreter(io: IO): BaseSchemeInterpreter[?]

    // we only need a lattice to compare the results of the interpreter with the expected answers
    lazy val lattice = SchemeConstantPropagationDomain.modularLattice.schemeLattice

    def testExpr(program: String, answer: Any): Unit =
        val text = SchemeParser.parseProgram(program)
        val l = lattice

        import l.Injector._

        val v = interpreter(new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> ""))).run(text, Timeout.start(Duration(30, SECONDS)))
        val result = v match
            case ConcreteValues.Value.Nil          => l.nil
            case ConcreteValues.Value.Str(s)       => l.string(s)
            case ConcreteValues.Value.Symbol(s)    => l.symbol(s)
            case ConcreteValues.Value.Integer(n)   => l.number(n)
            case ConcreteValues.Value.Real(r)      => l.real(r)
            case ConcreteValues.Value.Bool(b)      => l.bool(b)
            case ConcreteValues.Value.Character(c) => l.char(c)
            case _                                 => ???
        assert(
          l.tryCompare(answer, result).contains(0),
          s"Primitive computation test failed on program: $program with result $result (expected $answer)."
        )

class SchemeInterpreterR5RSCorrectnessTests extends SchemeR5RSInterpreterTests:
    def interpreter(io: IO) = new SchemeInterpreter((_: Identity, _: ConcreteValues.Value) => (), io)

class SchemeCPSInterpreterR5RSCorrectnessTests extends SchemeR5RSInterpreterTests:
    def interpreter(io: IO) = new CPSSchemeInterpreter((_: Identity, _: ConcreteValues.Value) => (), io)

class ConstantPropagationDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis(prg: SchemeExp) =
        new SchemeModFLocal(prg) with SchemeConstantPropagationDomain with SchemeModFLocalNoSensitivity with FIFOWorklistAlgorithm[SchemeExp]

class PowersetDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis(prg: SchemeExp) =
        new SchemeModFLocal(prg) with SchemePowersetDomain with SchemeModFLocalNoSensitivity with FIFOWorklistAlgorithm[SchemeExp]

class TypeDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis(prg: SchemeExp) =
        new SchemeModFLocal(prg) with SchemeTypeDomain with SchemeModFLocalNoSensitivity with FIFOWorklistAlgorithm[SchemeExp]
