package maf.cli.runnables

import maf.language.CScheme._
import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.scheme.modf._
import maf.modular.incremental._
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations._
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

object IncrementalRun extends App {

  // Runs the program with a concrete interpreter, just to check whether it makes sense (i.e., if the concrete interpreter does not error).
  // Useful when reducing a program when debugging the analysis.
  def interpretProgram(file: String): Unit = {
    val prog = CSchemeUndefiner.undefine(List(SchemePrelude.addPrelude(CSchemeParser.parse(Reader.loadFile(file)), Set("newline", "display"))))
    val i = new SchemeInterpreter((_, _) => (), stack = true)
    print("*")
    i.run(prog, Timeout.start(Duration(3, MINUTES)), Old)
    print("*")
    i.run(prog, Timeout.start(Duration(3, MINUTES)), New)
    println("*")
  }

  def modconcAnalysis(
      bench: String,
      config: IncrementalConfiguration,
      timeout: () => Timeout.T
    ): Unit = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new IncrementalModConcAnalysisCPLattice(text, config) {
      override def intraAnalysis(
          cmp: Component
        ) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis {
        override def analyzeWithTimeout(timeout: Timeout.T): Unit = {
          println(s"Analyzing $cmp")
          super.analyzeWithTimeout(timeout)
        }
      }
    }
    a.analyzeWithTimeout(timeout())
    print(a.finalResult)
    //a.updateAnalysis(timeout())
  }

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    def newAnalysis(text: SchemeExp, configuration: IncrementalConfiguration) =
      new IncrementalSchemeModFAnalysisCPLattice(text, configuration) with IncrementalLogging[SchemeExp] {
        override def focus(a: Addr): Boolean = a.toString.toLowerCase().contains("ret")

        override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
          with IncrementalSchemeModFBigStepIntra
          with IncrementalGlobalStoreIntraAnalysis
          //  with AssertionModFIntra
          with IncrementalLoggingIntra
      }

    println(s"***** $bench *****")
    interpretProgram(bench)
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = newAnalysis(text, IncrementalConfiguration.ci_di_wi)
    a.analyzeWithTimeout(timeout())
    a.updateAnalysis(timeout())
  }

  val modConcbenchmarks: List[String] = List()
  val modFbenchmarks: List[String] = List("test/changes/scheme/ring-rotate.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(30, SECONDS))

  modConcbenchmarks.foreach(modconcAnalysis(_, ci_di_wi, standardTimeout))
  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
}
