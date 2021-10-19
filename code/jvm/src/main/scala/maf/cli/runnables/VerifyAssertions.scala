package maf.cli.runnables

import maf.language.scheme._
import maf.modular.ModAnalysis
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.util.Reader
import maf.util.benchmarks.Timeout

object VerifyAssertions:

    def main(args: Array[String]): Unit = test(args(0))

    def test(program: String): Unit =
        val txt = Reader.loadFile(program)
        val prg = SchemeParser.parseProgram(txt)
        val analysis = new ModAnalysis(prg)
          with SchemeModFSemanticsM
          with SchemeAssertSemantics
          with StandardSchemeModFComponents
          with SchemeTypeDomain
          with SchemeModFKCallSiteSensitivity
          with LIFOWorklistAlgorithm[SchemeExp] {
          val k = 2

          override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with AssertionModFIntra
        }
        analysis.analyzeWithTimeout(Timeout.none)
        val failed = analysis.assertionsFailed
        println(s"There are ${failed.size} violations")
        failed.foreach(v => println(s"Violation of ${v._2} in component ${v._1}"))
