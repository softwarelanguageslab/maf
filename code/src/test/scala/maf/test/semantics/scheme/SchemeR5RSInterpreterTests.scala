package maf.test.semantics.scheme

import maf.domain.scheme.*
import maf.semantics.scheme.interpreter.*
import maf.syntax.*
import maf.syntax.scheme.*
import maf.test.SchemeR5RSTests
import maf.util.Timeout

import scala.concurrent.duration._

/** Checks the result of each R5RS test program, as computed by a concrete interpreter. */
trait SchemeR5RSInterpreterTests extends SchemeR5RSTests:

    def interpreter(io: IO): BaseSchemeInterpreter[?]

    // we only need a lattice to compare the results of the interpreter with the expected answers
    lazy val lattice = SchemeConstantPropagationDomain.modularLattice.schemeLattice

    def testExpr(program: String, answer: Any): Unit =
        val text = SchemeParser.parseProgram(program, Source.Text)
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
    def interpreter(io: IO) = new SchemeInterpreter((_: Option[Position], _: ConcreteValues.Value) => (), io)

class SchemeCPSInterpreterR5RSCorrectnessTests extends SchemeR5RSInterpreterTests:
    def interpreter(io: IO) = new CPSSchemeInterpreter((_: Option[Position], _: ConcreteValues.Value) => (), io)
