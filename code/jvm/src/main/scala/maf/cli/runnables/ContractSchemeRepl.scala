package maf.cli.runnables

import maf.language.ContractScheme.*
import maf.language.scheme.*
import maf.language.ContractScheme.interpreter.*
import scala.io.StdIn
import maf.util.benchmarks.Timeout

/**
 * A read-eval-print loop for the concrete ContractScheme interpreter.
 *
 * Can be used to check how the concrete interpreter parses and executes a program and to determine whether it is correct
 */
object ContractSchemeRepl:
    private def parseProgram(input: String): SchemeExp =
      ContractSchemeParser.parse(input)

    def main(args: Array[String]): Unit =
        val interpreter = ContractSchemeInterpreter()
        var running = true
        while running do
            print("> ")
            val input = StdIn.readLine
            if input == ":q" then running = false
            else
                try {
                  val program = parseProgram(input)
                  try {
                    println(interpreter.run(program, Timeout.none))
                  } catch {
                    case e => println(s"unexpected error $e")
                  }
                } catch {
                  case e =>
                    println(s"parse error $e")
                    e.printStackTrace
                }
