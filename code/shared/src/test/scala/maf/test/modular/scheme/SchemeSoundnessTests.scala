package maf.test.modular.scheme

import java.util.concurrent.TimeoutException
import org.scalatest.Tag
import maf.core._
import maf.language.CScheme._
import maf.language.scheme.interpreter.ConcreteValues._
import maf.language.scheme._
import maf.language.scheme.interpreter._
import maf.language.scheme.lattices.SchemeOp
import maf.language.scheme.primitives.SchemePrelude
import maf.modular._
import maf.modular.scheme._
import maf.test._
import maf.util._
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

trait SchemeSoundnessTests extends SchemeBenchmarkTests {
  // analysis must support basic Scheme semantics
  type Analysis = ModAnalysis[SchemeExp] with AnalysisResults[SchemeExp] with SchemeDomain
  // the analysis that is used to analyse the programs
  def name: String
  def analysis(b: SchemeExp): Analysis
  // the timeout and max number of concrete runs for a single benchmark program (default: 1min.)
  def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(1, MINUTES))
  def concreteTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(1, MINUTES))
  def concreteRuns(b: Benchmark): Int = 1 // for highly non-deterministic programs, a higher value is recommended
  // the actual testing code
  protected def runInterpreter(
      i: SchemeInterpreter,
      p: SchemeExp,
      t: Timeout.T
    ): Value =
    i.run(p, t) // If there are code changes in the file, runs the "new" version by default (ensures compatibility with files containing changes).
  protected def evalConcrete(originalProgram: SchemeExp, benchmark: Benchmark): Map[Identity, Set[Value]] = {
    val preluded = SchemePrelude.addPrelude(originalProgram)
    val program = CSchemeUndefiner.undefine(List(preluded))
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    try for (_ <- 1 to times) {
      val interpreter = new SchemeInterpreter((i, v) => idnResults += (i -> (idnResults(i) + v)),
                                              io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> ""))
      )
      val finalResult = runInterpreter(interpreter, program, timeout)
      idnResults += program.idn -> (idnResults(program.idn) + finalResult)
    } catch {
      case _: TimeoutException =>
        alert(s"Concrete evaluation of $benchmark timed out.")
      case ChildThreadDiedException(_) =>
        alert(s"Concrete evaluation of $benchmark aborted due to a fatal crash in a child thread.")
      case e: VirtualMachineError =>
        System.gc()
        alert(s"Concrete evaluation of $benchmark failed with $e")
    }
    idnResults
  }
  protected def runAnalysis(program: SchemeExp, benchmark: Benchmark): Analysis =
    try {
      // analyze the program using a ModF analysis
      val anl = analysis(program)
      val timeout = analysisTimeout(benchmark)
      anl.analyzeWithTimeout(timeout)
      assume(anl.finished, "Analysis timed out")
      anl
    } catch {
      case e: VirtualMachineError =>
        System.gc()
        cancel(s"Analysis of $benchmark encountered an error: $e")
    }
  protected def checkSubsumption(analysis: Analysis)(v: Value, abs: analysis.Value): Boolean = {
    val lat = analysis.lattice
    v match {
      case Value.Undefined(_)  => true
      case Value.Unbound(_)    => true
      case Value.Void          => lat.subsumes(abs, lat.void)
      case Value.Clo(lam, _)   => lat.getClosures(abs).exists(_._1.idn == lam.idn)
      case Value.Primitive(p)  => lat.getPrimitives(abs).exists(_ == p)
      case Value.Str(s)        => lat.subsumes(abs, lat.string(s))
      case Value.Symbol(s)     => lat.subsumes(abs, lat.symbol(s))
      case Value.Integer(i)    => lat.subsumes(abs, lat.number(i))
      case Value.Real(r)       => lat.subsumes(abs, lat.real(r))
      case Value.Bool(b)       => lat.subsumes(abs, lat.bool(b))
      case Value.Character(c)  => lat.subsumes(abs, lat.char(c))
      case Value.Nil           => lat.subsumes(abs, lat.nil)
      case Value.Pointer(_)    => lat.getPointerAddresses(abs).nonEmpty
      case Value.Thread(_)     => lat.getThreads(abs).nonEmpty
      case Value.InputPort(h)  => lat.subsumes(abs, lat.op(SchemeOp.MakeInputPort)(List(lat.string(h.abstractName))).getOrElse(lat.bottom))
      case Value.OutputPort(h) => lat.subsumes(abs, lat.op(SchemeOp.MakeOutputPort)(List(lat.string(h.abstractName))).getOrElse(lat.bottom))
      case Value.EOF           => lat.subsumes(abs, lat.charTop)
      case v                   => throw new Exception(s"Unknown concrete value type: $v.")
    }
  }

  protected def compareResults(
      analysis: Analysis,
      concreteResults: Map[Identity, Set[Value]],
      message: String = ""
    ): Unit = {
    val analysisResults = analysis.resultsPerIdn
    concreteResults.foreach { case (idn, concreteValues) =>
      val abstractValues = analysisResults.getOrElse(idn, Set.empty)
      concreteValues.foreach { concreteValue =>
        if (!abstractValues.exists(checkSubsumption(analysis)(concreteValue, _))) {
          val failureMsg =
            s"""
            | Intermediate result at $idn is unsound:
            | - concrete value: $concreteValue
            | - abstract values: $abstractValues
            """.stripMargin
          if (message.isEmpty) {
            fail(failureMsg)
          } else {
            fail(s"$message > $failureMsg")
          }
        }
      }
    }
  }

  // indicate if a benchmark is slow or not
  def isSlow(b: Benchmark) = false

  def testTags(b: Benchmark): Seq[Tag] =
    if (isSlow(b)) {
      Seq(SoundnessTest, SlowTest)
    } else {
      Seq(SoundnessTest)
    }

  def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = CSchemeParser.parse(content)
      // run the program using a concrete interpreter
      val concreteResults = evalConcrete(program, benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program, benchmark)
      // check if the analysis results soundly (over-)approximate the concrete results
      compareResults(anl, concreteResults)
    }
}
