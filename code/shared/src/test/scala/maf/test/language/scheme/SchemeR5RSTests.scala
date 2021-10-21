package maf.test.language.scheme

import org.scalatest.propspec.AnyPropSpec
import maf.core._
import maf.language.scheme._
import maf.language.scheme.interpreter._
import maf.language.scheme.lattices.SchemeLattice
import maf.language.scheme.primitives._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular._
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.test.{PrimitiveTest, SchemeR5RSBenchmarks}
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

trait SchemeR5RSTests extends AnyPropSpec:

    type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemanticsM
    type V
    type L = SchemeLattice[V, _ <: Address]

    def analysis(text: SchemeExp): Analysis

    def testExpr(program: String, answer: Any): Unit =
        val text = SchemeParser.parseProgram(program)
        val a = analysis(text)
        val l = a.lattice.asInstanceOf[L]

        import l.Injector._

        a.analyzeWithTimeout(Timeout.start(Duration(30, SECONDS)))
        // All R5RS tests should terminate, no matter the analysis, because they're so simple.
        assert(a.finished, s"Analysis of $program should finish within the given time bound out.")
        val result = a.finalResult.asInstanceOf[V]
        assert(l.subsumes(result, answer), s"Primitive computation test failed on program: $program with result $result (expected $answer).")

    SchemeR5RSBenchmarks.bench.foreach { case (e, a) => property(s"Primitive in $e is correct.", PrimitiveTest)(testExpr(e, a)) }

class SchemeInterpreterR5RSCorrectnessTests extends SchemeR5RSTests:

    def analysis(text: SchemeExp) =
      // Not really clean, we only want a proper ConstantPropagationLattice definition
      new SimpleSchemeModFAnalysis(text) with SchemeConstantPropagationDomain with SchemeModFNoSensitivity with LIFOWorklistAlgorithm[SchemeExp]

    override def testExpr(program: String, answer: Any): Unit =
        val text = SchemeParser.parseProgram(program)
        val a = analysis(text)
        val l = a.lattice.asInstanceOf[L]

        import l.Injector._

        val interpreter =
          new SchemeInterpreter((_: Identity, _: ConcreteValues.Value) => (), io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")))
        val v = interpreter.run(text, Timeout.start(Duration(30, SECONDS)))
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

class SchemeCPSInterpreterR5RSCorrectnessTests extends SchemeR5RSTests:

    def analysis(text: SchemeExp) =
      // Not really clean, we only want a proper ConstantPropagationLattice definition
      new SimpleSchemeModFAnalysis(text) with SchemeConstantPropagationDomain with SchemeModFNoSensitivity with LIFOWorklistAlgorithm[SchemeExp]

    override def testExpr(program: String, answer: Any): Unit =
        val text = SchemeParser.parseProgram(program)
        val a = analysis(text)
        val l = a.lattice.asInstanceOf[L]

        import l.Injector._

        val interpreter =
          new CPSSchemeInterpreter((_: Identity, _: ConcreteValues.Value) => (),
                                   io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> ""))
          )
        val v = interpreter.run(text, Timeout.start(Duration(30, SECONDS)))
        val result = v match
            case ConcreteValues.Value.Nil          => l.nil
            case ConcreteValues.Value.Str(s)       => l.string(s)
            case ConcreteValues.Value.Symbol(s)    => l.symbol(s)
            case ConcreteValues.Value.Integer(n)   => l.number(n)
            case ConcreteValues.Value.Real(r)      => l.real(r)
            case ConcreteValues.Value.Bool(b)      => l.bool(b)
            case ConcreteValues.Value.Character(c) => l.char(c)
            case _                                 => ???
        assert(l.tryCompare(answer, result).contains(0),
               s"Primitive computation test failed on program: $program with result $result (expected $answer)."
        )

class ConstantPropagationBigStepModFR5RSCorrectnessTests extends SchemeR5RSTests:
    def analysis(
        text: SchemeExp
      ) = new SimpleSchemeModFAnalysis(text) with SchemeConstantPropagationDomain with SchemeModFNoSensitivity with LIFOWorklistAlgorithm[SchemeExp]

class PowersetBigStepModFR5RSCorrectnessTests extends SchemeR5RSTests:
    def analysis(
        text: SchemeExp
      ) = new SimpleSchemeModFAnalysis(text) with SchemePowersetDomain with SchemeModFFullArgumentSensitivity with LIFOWorklistAlgorithm[SchemeExp]

class TypeBigStepModFR5RSCorrectnessTests extends SchemeR5RSTests:
    def analysis(
        text: SchemeExp
      ) = new SimpleSchemeModFAnalysis(text) with SchemeTypeDomain with SchemeModFNoSensitivity with LIFOWorklistAlgorithm[SchemeExp]
