package maf.cli.experiments.clients

import maf.modular.scheme.modf.SimpleSchemeModFAnalysis
import maf.core.Identity
import maf.modular.ModAnalysis
import maf.language.scheme.*
import maf.modular.scheme.modf.SchemeModFSemanticsM
import maf.modular.scheme.modf.BigStepModFSemantics
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.worklist.FIFOWorklistAlgorithm

/** A dead code detection analysis, is a client analysis of the ModF analysis */
trait DeadcodeDetection extends BigStepModFSemantics:
    var visitedIdn: Set[Identity] = Set()

    override def intraAnalysis(cmp: Component): DeadcodeDetectionIntra

    abstract class DeadcodeDetectionIntra(cmp: Component) extends IntraAnalysis(cmp) with BigStepModFIntra:
        override def eval(exp: SchemeExp): EvalM[Value] =
            visitedIdn = visitedIdn + exp.idn
            super.eval(exp)

object DeadcodeDetection:
    type Analysis = DeadcodeDetection

    /**
     * Creates a dead code detection analysis
     *
     * @param program
     *   the program to analyze
     */
    private def createAnalysis(program: SchemeExp): DeadcodeDetection =
      new ModAnalysis[SchemeExp](program)
        with DeadcodeDetection
        with StandardSchemeModFComponents
        with SchemeModFSemanticsM
        with SchemeModFNoSensitivity
        with BigStepModFSemantics
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp]:

          override def intraAnalysis(cmp: Component): DeadcodeDetectionIntra =
            new DeadcodeDetectionIntra(cmp) {}

    /** Parses the given program text to a SchemeExp */
    def parseProgram(txt: String): SchemeExp =
      SchemeParser.parseProgram(txt)

    /**
     * Returns a set of expressions that was not visited during the analysis.
     *
     * These expressions are, by definition dead code as the analysis will always visit paths that are possibly reachable during execution due to its
     * sound overapproximations
     */
    def run(program: String): Set[Identity] =
        val exp = parseProgram(program)
        val analysis = createAnalysis(exp)
        analysis.analyze()
        exp.allSubexpressions.map(_.idn).toSet -- analysis.visitedIdn
