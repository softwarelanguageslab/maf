package maf.test.semantics.scheme

import maf.semantics.scheme.interpreter.*
import maf.syntax.{Position, Source}
import maf.syntax.scheme.*
import maf.util.Timeout

import org.scalatest.propspec.AnyPropSpec

/** Checks the values that the concrete interpreters read from ports */
class ReadTests extends AnyPropSpec:
    val interpreters: List[(String, IO => BaseSchemeInterpreter[?])] = List(
      "SchemeInterpreter" -> (io => new SchemeInterpreter((_: Option[Position], _: ConcreteValues.Value) => (), io)),
      "CPSSchemeInterpreter" -> (io => new CPSSchemeInterpreter((_: Option[Position], _: ConcreteValues.Value) => (), io))
    )

    val programs: List[(String, BigInt)] = List(
      """(vector-ref (read (open-input-string "#(1 2 3)")) 1)""" -> 2,
      """(vector-length (read (open-input-string "#(1 (2 3) #(4))")))""" -> 3,
      """(car (vector-ref (read (open-input-string "#(1 (2 3))")) 1))""" -> 2
    )

    for
        (name, interpreter) <- interpreters
        (program, result) <- programs
    do
        property(s"$name reads the vector in $program") {
            val prg = SchemeParser.parseProgram(program, Source.Text)
            assert(interpreter(new FileIO(Map())).run(prg, Timeout.none) == ConcreteValues.Value.Integer(result))
        }
