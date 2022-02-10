package maf.cli.experiments.interpreter

import maf.language.scheme.SchemeParser
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.ContractScheme.interpreter.ContractSchemeInterpreter
import scala.util.control.TailCalls.TailRec
import maf.language.scheme.*
import maf.language.change.CodeVersion.{New, Version}
import maf.util.benchmarks.Timeout
import maf.language.ContractScheme.interpreter.RandomInputGenerator
import maf.language.ContractScheme.ContractSchemeParser

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

    override def eval(e: SchemeExp, env: Env, timeout: Timeout.T, version: Version): TailRec[Value] =
        visitedLines = visitedLines + e.idn.pos.line
        super.eval(e, env, timeout, version)

    def compute(program: String): Double =
        val parsedProgram = parseProgram(program)
        run(parsedProgram, Timeout.none)
        this.visitedLines.size / program.split('\n').size

trait ScvParser extends Coverage:
    override def parseProgram(program: String): SchemeExp =
      ContractSchemeParser.parse(program)

/** Measures the coverage of the given program */
class CodeCoverage(program: String):
    def scvSelectRandomGenerator(programPath: String): RandomInputGenerator = ???

    /**
     * Computes the line coverage of the given ContractScheme program.
     *
     * This will use a predefined set of random inputs for the given program path (if one is available on the file system).
     *
     * This method will look at the following locations for a file containing random inputs:
     *
     *   - input/PATH_SEPERATORS_REPLACED_WITH_UNDERSCORES.json
     *
     * The file will be parsed according to maf.language.ContractScheme.interpreter.RandomInputsFromFile
     *
     * @param programPath
     *   the original path of the program
     *
     * @see
     *   maf.language.ContractScheme.interpreter.RandomInputsFromFile
     */
    def scvLineCoverage(programPath: String): Double =
        val generator = scvSelectRandomGenerator(programPath)
        val interpreter = new ContractSchemeInterpreter(generator = Some(generator)) with LineCoverageInterpreter with ScvParser {}
        interpreter.compute(program)
