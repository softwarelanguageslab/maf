package maf.test.language.scheme

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Identity
import maf.language.CScheme._
import maf.language.scheme._
import maf.language.scheme.interpreter._
import maf.language.scheme.primitives.SchemePrelude
import maf.test.InterpreterTest
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.propspec.AnyPropSpec

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._

class InterpreterTests() extends AnyPropSpec {

  val benchmarks: Set[String] = SchemeBenchmarkPrograms.sequentialBenchmarks

  val interpreter = new SchemeInterpreter((_: Identity, _: ConcreteValues.Value) => (), io = new EmptyIO())
  val CPSinterpreter = new CPSSchemeInterpreter((_: Identity, _: ConcreteValues.Value) => (), io = new EmptyIO())

  def onBenchmark(benchmark: String): Unit = {
    val program: SchemeExp = CSchemeUndefiner.undefine(List(SchemePrelude.addPrelude(CSchemeParser.parse(Reader.loadFile(benchmark)))))
    property(s"The Scheme interpreters give the same result for $benchmark", InterpreterTest) {
      try {
        val r1: ConcreteValues.Value = interpreter.run(program, Timeout.start(Duration(60, SECONDS)))
        val r2: ConcreteValues.Value = CPSinterpreter.run(program, Timeout.start(Duration(60, SECONDS)))
        (r1, r2) match {
          case (ConcreteValues.Value.Pointer(p1), ConcreteValues.Value.Pointer(p2)) =>
            assert(interpreter.store(p1) == CPSinterpreter.store(p2)) // Approximation.
          case (ConcreteValues.Value.Thread(t1), ConcreteValues.Value.CThread(t2)) => cancel(s"Cannot compare thread results of $benchmark.")
          case (v1, v2)                                                            => assert(v1 == v2)
        }
        assert(r1 == r2)
      } catch {
        case _: TimeoutException    => cancel(s"One of the interpreters timed out on $benchmark.")
        case e: VirtualMachineError => cancel(s"Interpretation of $benchmark encountered an error: $e.")
      }
    }
  }

  benchmarks.foreach(onBenchmark)

}
