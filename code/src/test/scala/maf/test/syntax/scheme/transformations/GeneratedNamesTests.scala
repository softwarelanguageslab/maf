package maf.test.syntax.scheme.transformations

import maf.semantics.scheme.interpreter.*
import maf.syntax.{Position, Source}
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.{SchemeMutableVarBoxer, SchemePrelude}
import maf.util.Timeout

import org.scalatest.propspec.AnyPropSpec

/** Checks that the names that the program transformations generate do not clash with the names in programs */
class GeneratedNamesTests extends AnyPropSpec:
    // adds the prelude to the given program, boxes and undefines it, and runs it
    def run(program: String): ConcreteValues.Value =
        val prelud = SchemePrelude.addPrelude(SchemeParser.parse(program, Source.Text))
        val prepared = SchemeParser.undefine(SchemeMutableVarBoxer.transform(prelud))
        val interpreter = new SchemeInterpreter((_: Option[Position], _: ConcreteValues.Value) => (), io = new EmptyIO())
        interpreter.run(prepared, Timeout.none)

    val programs: List[(String, BigInt)] = List(
      // a variable with the name that the undefiner used to give to the first expression at the top level
      "(define _0 5) (+ _0 1)" -> 6,
      // a variable with the name that the boxer used to give to the reference of a mutable variable `x`
      "(define (f x __ref_var_x) (set! x (+ x 1)) (+ x __ref_var_x)) (f 1 10)" -> 12
    )

    programs.foreach { (program, result) =>
        property(s"The transformations of $program do not change its result") {
            assert(run(program) == ConcreteValues.Value.Integer(result))
        }
    }
