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

  /** Comparison of values that can also handle pointers. */
  def compareValues(v1: ConcreteValues.Value, v2: ConcreteValues.Value): Option[Boolean] = (v1, v2) match {
    case (ConcreteValues.Value.Cons(car1, cdr1), ConcreteValues.Value.Cons(car2, cdr2)) =>
      compareValues(car1, car2).flatMap(b => if (b) compareValues(cdr1, cdr2) else Some(b))
    case (ConcreteValues.Value.Pointer(p1), ConcreteValues.Value.Pointer(p2)) =>
      compareValues(interpreter.store(p1), CPSinterpreter.store(p2)) // Check the values in the store.
    case (ConcreteValues.Value.Thread(_), ConcreteValues.Value.CThread(_)) => None // Cannot be compared.
    case (ConcreteValues.Value.Vector(s1, m1, i1), ConcreteValues.Value.Vector(s2, m2, i2)) =>
      if (s1 == s2 && i1 == i2)
        m1.toList.map(_._2).zip(m2.toList.map(_._2)).foldLeft[Option[Boolean]](Some(true)) { case (prev, (vv1, vv2)) =>
          prev.flatMap(b => if (b) compareValues(vv1, vv2) else Some(b))
        }
      else Some(false)
    case _ => Some(v1 == v2)
  }

  def onBenchmark(benchmark: String): Unit = {
    val program: SchemeExp = CSchemeUndefiner.undefine(List(SchemePrelude.addPrelude(CSchemeParser.parse(Reader.loadFile(benchmark)))))
    property(s"The Scheme interpreters give the same result for $benchmark", InterpreterTest) {
      try {
        val r1: ConcreteValues.Value = interpreter.run(program, Timeout.start(Duration(60, SECONDS)))
        val r2: ConcreteValues.Value = CPSinterpreter.run(program, Timeout.start(Duration(60, SECONDS)))
        compareValues(r1, r2) match {
          case Some(b) => assert(b)
          case None    => cancel(s"Cannot compare results of $benchmark as they involve threads.")
        }
      } catch {
        case _: TimeoutException    => cancel(s"One of the interpreters timed out on $benchmark.")
        case e: VirtualMachineError => cancel(s"Interpretation of $benchmark encountered an error: $e.")
      }
    }
  }

  benchmarks.foreach(onBenchmark)

}
