package maf.cli.experiments.interpreter

import maf.language.scheme.SchemeParser
import maf.language.scheme.interpreter.SchemeInterpreter
import scala.util.control.TailCalls.TailRec
import maf.language.scheme.*
import maf.util.benchmarks.Timeout
import maf.util.Reader
import scala.concurrent.duration.*
import java.util.concurrent.TimeoutException

/** Objects implementing this trait must provide a "compute" method that returns the code coverage (between 0 and 1) of the program. */
trait Coverage:
    /**
     * Compute the code coverage for the given expression.
     *
     * @param program
     *   the program to run using the concrete interpreter and computing the code coverage for
     * @return
     *   the code coverage as a fraction of some metric (i.e. lines for line coverage).
     */
    def compute(program: String): Double

    def parseProgram(program: String): SchemeExp =
        SchemeParser.parseProgram(program)

/** An interpreter for computing the line coverage */
trait LineCoverageInterpreter extends SchemeInterpreter, Coverage:
    import maf.language.scheme.interpreter.ConcreteValues.*

    var visitedLines: Set[Int] = Set()

    override def eval(e: SchemeExp, env: Env, timeout: Timeout.T): TailRec[Value] =
        if e.idn.pos.line >= 0 then visitedLines = visitedLines + e.idn.pos.line // line can be -1 in case of Identity.none
        super.eval(e, env, timeout)

    def compute(program: String): Double =
        val parsedProgram = parseProgram(program)
        try run(parsedProgram, Timeout.start(15.seconds))
        catch { case _: TimeoutException => () }

        // even if the execution timed-out we will run with the coverage results we already obtained
        this.visitedLines.size.toDouble / (program.linesIterator
            .filterNot(line =>
                // ignore lines that start with a comment, or that do not contain any characters except for whitespaces
                line.trim.nn.startsWith(";") || line.trim.nn.size == 0
            )
            .size
            .toDouble)