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
import scala.reflect.ClassTag
import maf.language.AScheme.interpreter.FutureCannotBeSent

class ASchemeInterpreterTests extends AnyPropSpec:
    val concreteRuns: Int = 1000

    given Logger.Logger = Logger.DisabledLog()
    def program(filename: String, expected: ConcreteValues.Value): Unit =
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

    def expectError[E <: Exception](filename: String)(using e: ClassTag[E]): Unit =
        property(s"$filename should result in exception ${e.runtimeClass.getSimpleName()}") {
            val program = Reader.loadFile(filename)
            val parsed = ASchemeParser.parseProgram(program)
            // run it a few times, all of them should fail!
            (0 until concreteRuns).foreach { i =>
                val interpreter = CPSASchemeInterpreter()
                try
                    interpreter.run(parsed, Timeout.none)
                    fail(s"run $i did not end in an error, return value is ${interpreter.getReturnValue}")
                catch case _: E => ()
            }
        }

    program("test/concurrentScheme/actorfutures/player.rkt", ConcreteValues.Value.Integer(42))
    expectError[FutureCannotBeSent]("test/concurrentScheme/actorfutures/send_future_as_message.rkt")
