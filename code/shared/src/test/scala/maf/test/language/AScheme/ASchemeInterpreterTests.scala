package maf.test.language.AScheme

import org.scalatest.propspec.AnyPropSpec
import maf.language.scheme.interpreter.ConcreteValues
import maf.language.scheme.*
import maf.core.Identity
import maf.language.AScheme.interpreter.CPSASchemeInterpreter.apply
import maf.language.AScheme.interpreter.CPSASchemeInterpreter
import maf.language.AScheme.ASchemeParser
import maf.util.Logger
import maf.util.Reader
import maf.util.benchmarks.Timeout

class ASchemeInterpreterTests extends AnyPropSpec:
    val concreteRuns: Int = 1000

    def program(filename: String, expected: ConcreteValues.Value): Unit =
        given Logger.Logger = Logger.DisabledLog()
        property(s"$filename should return $expected from the main actor") {
            val program = Reader.loadFile(filename)
            val parsed = ASchemeParser.parseProgram(program)
            // run it a few times to be sure
            (0 until concreteRuns).foreach { _ =>
                val interpreter = CPSASchemeInterpreter()
                interpreter.run(parsed, Timeout.none)
                assert(interpreter.getReturnValue == expected)
            }
        }

    program("test/concurrentScheme/actorfutures/player.rkt", ConcreteValues.Value.Integer(42))
