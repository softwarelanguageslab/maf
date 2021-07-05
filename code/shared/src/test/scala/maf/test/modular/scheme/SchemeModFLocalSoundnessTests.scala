package maf.test.modular.scheme

import java.util.concurrent.TimeoutException
import org.scalatest.Tag
import maf.core._
import maf.language.CScheme._
import maf.language.scheme.interpreter.ConcreteValues._
import maf.language.scheme._
import maf.language.scheme.interpreter._
import maf.language.scheme.lattices._
import maf.language.scheme.primitives._
import maf.modular.scheme.modflocal._
import maf.test._
import maf.util._
import maf.util.benchmarks.Timeout
import maf.modular.scheme._
import maf.modular.worklist._

import scala.concurrent.duration._

trait SchemeModFLocalSoundnessTests extends SchemeBenchmarkTests {
  // analysis must support basic Scheme semantics
  type Analysis = SchemeModFLocal
  // the analysis that is used to analyse the programs
  def name: String
  def analysis(b: SchemeExp): Analysis
  // the timeout and max number of concrete runs for a single benchmark program (default: 1min.)
  def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(1, MINUTES))
  def concreteTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(1, MINUTES))
  def concreteRuns(b: Benchmark): Int = 1 // for highly non-deterministic programs, a higher value is recommended
  // TODO: factor out some of this code that is common to other soundness tests.
  protected def evalConcrete(originalProgram: SchemeExp, benchmark: Benchmark): (Set[Value], Map[Identity, Set[Value]]) = {
    val preluded = SchemePrelude.addPrelude(originalProgram)
    val program = CSchemeUndefiner.undefine(List(preluded))
    var endResults = Set[Value]()
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    try for (_ <- 1 to times) {
      val interpreter = new SchemeInterpreter((i, v) => idnResults += (i -> (idnResults(i) + v)),
                                              io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> ""))
      )
      endResults += interpreter.run(program, timeout)
    } catch {
      case _: TimeoutException =>
        alert(s"Concrete evaluation of $benchmark timed out.")
      case ChildThreadDiedException(_) =>
        alert(s"Concrete evaluation of $benchmark aborted due to a fatal crash in a child thread.")
      case e: VirtualMachineError =>
        System.gc()
        alert(s"Concrete evaluation of $benchmark failed with $e")
    }
    (endResults, idnResults)
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

  protected def compareResult(
      anl: Analysis,
      vls: Set[Value],
      msg: String = ""
    ): Unit = {
    val results = anl.visited.collect { case anl.HaltComponent(vlu, _) =>
      vlu
    }
    vls.foreach { cRes =>
      if (!results.exists(aRes => checkSubsumption(anl)(cRes, aRes))) {
        val failureMsg =
          s"""Program result is unsound:
            | - concrete value: $cRes
            | - abstract values: $results
          """.stripMargin
        if (msg.isEmpty) {
          fail(failureMsg)
        } else {
          fail(s"$msg > $failureMsg")
        }
      }
    }
  }

  private def addBindings[K, V](to: Map[K, Set[V]], from: Map[K, V]): Map[K, Set[V]] =
    from.foldLeft(to) { case (acc, (key, vlu)) =>
      acc + (key -> (acc.getOrElse(key, Set.empty) + vlu))
    }

  protected def compareIdentities(
      anl: Analysis,
      conIdn: Map[Identity, Set[Value]],
      msg: String = ""
    ): Unit = {
    val stores = anl.visited
      .collect {
        case anl.HaltComponent(_, sto)    => sto
        case anl.CallComponent(_, _, sto) => sto
        case anl.KontComponent(_, _, sto) => sto
      }
      .map {
        _.content
          .groupBy(_._1.idn)
          .view
          .mapValues(m => m.values.collect { case anl.V(vlu) => vlu })
          .mapValues(v => anl.lattice.join(v))
          .toMap
      }
    val absIdn =
      stores
        .foldLeft(Map.empty[Identity, Set[anl.Value]]) { (acc, sto) =>
          addBindings(acc, sto)
        }
        .withDefaultValue(Set.empty)
    conIdn.foreach { case (idn, values) =>
      values.foreach { cRes =>
        val results = absIdn(idn)
        if (!results.exists(aRes => checkSubsumption(anl)(cRes, aRes))) {
          val failureMsg =
            s"""Intermediate result at $idn is unsound:
            | - concrete value: $cRes
            | - abstract values: $results
            """.stripMargin
          if (msg.isEmpty) {
            fail(failureMsg)
          } else {
            fail(s"$msg > $failureMsg")
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
      val (cResult, cPosResults) = evalConcrete(program, benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program, benchmark)
      // check if the final result of the analysis soundly approximates the final result of concrete evaluation
      compareResult(anl, cResult)
      // check if the intermediate results at various program points are soundly approximated by the analysis
      compareIdentities(anl, cPosResults)
    }
}

class SchemeModFLocalInsensitiveSoundnessTests extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks {
  def name = "MODF LOCAL (context-insensitive)"
  def analysis(prg: SchemeExp): SchemeModFLocal =
    new SchemeModFLocal(prg) with SchemeConstantPropagationDomain with SchemeModFLocalNoSensitivity with FIFOWorklistAlgorithm[SchemeExp]
}

class SchemeModFLocalCallSiteSensitiveSoundnessTests extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks {
  def name = "MODF LOCAL (call-site sensitive)"
  def analysis(prg: SchemeExp): SchemeModFLocal =
    new SchemeModFLocal(prg) with SchemeConstantPropagationDomain with SchemeModFLocalCallSiteSensitivity with FIFOWorklistAlgorithm[SchemeExp] {
      val k = 1
    }
}
 