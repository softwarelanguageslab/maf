package maf.test.language.scheme

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Identity
import maf.language.CScheme._
import maf.language.scheme._
import maf.language.scheme.interpreter.ConcreteValues.Addr
import maf.language.scheme.interpreter._
import maf.language.scheme.primitives.SchemePrelude
import maf.test.{InterpreterTest, SlowTest}
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.propspec.AnyPropSpec

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._

class InterpreterTests() extends AnyPropSpec:

    val benchmarks: Set[String] = SchemeBenchmarkPrograms.sequentialBenchmarks -- Set(
      "test/R5RS/various/my-test.scm" // Result depends on random numbers.
    )

    val interpreter = new SchemeInterpreter((_: Identity, _: ConcreteValues.Value) => (), io = new EmptyIO())
    val CPSinterpreter = new CPSSchemeInterpreter((_: Identity, _: ConcreteValues.Value) => (), io = new EmptyIO())

    /** Comparison of values that can also handle pointers. */
    def compareValues(
        v1: ConcreteValues.Value,
        v2: ConcreteValues.Value,
        visited: Map[Addr, Addr] = Map()
      ): Option[Boolean] = (v1, v2) match
        case (ConcreteValues.Value.Cons(car1, cdr1), ConcreteValues.Value.Cons(car2, cdr2)) =>
          compareValues(car1, car2, visited).flatMap(b => if b then compareValues(cdr1, cdr2, visited) else Some(b))
        case (ConcreteValues.Value.Pointer(p1), ConcreteValues.Value.Pointer(p2)) =>
          compareValues(interpreter.store(p1), CPSinterpreter.store(p2), visited) // Check the values in the store.
        case (ConcreteValues.Value.Clo(l1, e1), ConcreteValues.Value.Clo(l2, e2)) =>
          if l1 == l2 && e1.keySet == e2.keySet then
              // When comparing procedures, the addresses in the environment may differ. Therefore, for every variable, we have
              // to check the value at the given address. However, this may cause looping. Therefore, we keep a visited map
              // that breaks loops and considers them as verified (which is the case if all other comparisons succeed).
              e1.toList.map(_._2).zip(e2.toList.map(_._2)).foldLeft[Option[Boolean]](Some(true)) { case (prev, (a1, a2)) =>
                prev.flatMap(b =>
                  if b then
                      if visited
                            .contains(a1)
                      then // TODO Maybe visited should be a set of tuples, and when (a1, a2) is not present, the check should continue?
                          Some(visited(a1) == a2) // Perform an extra check that the cycle is "real", and hence that both addresses are the same.
                      else compareValues(interpreter.store(a1), CPSinterpreter.store(a2), visited + (a1 -> a2))
                  else Some(b)
                )
              }
          else Some(false)
        case (ConcreteValues.Value.Thread(_), ConcreteValues.Value.CThread(_)) => None // Cannot be compared.
        case (ConcreteValues.Value.Vector(s1, m1, i1), ConcreteValues.Value.Vector(s2, m2, i2)) =>
          if s1 == s2 && i1 == i2 then
              m1.toList.map(_._2).zip(m2.toList.map(_._2)).foldLeft[Option[Boolean]](Some(true)) { case (prev, (vv1, vv2)) =>
                prev.flatMap(b => if b then compareValues(vv1, vv2, visited) else Some(b))
              }
          else Some(false)
        case _ => Some(v1 == v2)

    def onBenchmark(benchmark: String): Unit =
        val program: SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(benchmark))
        property(s"The Scheme interpreters give the same result for $benchmark", InterpreterTest, SlowTest) {
          try
              // TODO (maybe): run in parallel?
              val r1: ConcreteValues.Value = interpreter.run(program, Timeout.start(Duration(2, MINUTES)))
              val r2: ConcreteValues.Value = CPSinterpreter.run(program, Timeout.start(Duration(2, MINUTES)))
              compareValues(r1, r2) match
                  case Some(b) =>
                    assert(b, s"The return value of the CPS interpreter ($r2) did not match the return value of the recursive interpreter ($r1).")
                  case None => cancel(s"Cannot compare results of $benchmark as they involve threads.")
          catch
              case _: TimeoutException    => cancel(s"One of the interpreters timed out on $benchmark.")
              case e: VirtualMachineError => cancel(s"Interpretation of $benchmark encountered an error: $e.")
        }

    benchmarks.foreach(onBenchmark)
