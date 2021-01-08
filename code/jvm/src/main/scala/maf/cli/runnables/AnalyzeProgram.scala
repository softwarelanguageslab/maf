package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.modular.{DependencyTracking, ModAnalysis}
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modf._
import maf.modular.scheme.ssmodconc._
import maf.modular.worklist.{FIFOWorklistAlgorithm, LIFOWorklistAlgorithm}
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

object AnalyzeProgram extends App {
  def one(bench: String, timeout: () => Timeout.T): Unit = {
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    /* val a = new ModAnalysis(text) with KKallocModConc with SchemeConstantPropagationDomain
    with LIFOWorklistAlgorithm[SchemeExp] {
      val k = 1

      override def intraAnalysis(component: SmallStepModConcComponent) =
        new IntraAnalysis(component) with KCFAIntra
    }*/

    val analysis = new SimpleSchemeModFAnalysis(text)
      with SchemeConstantPropagationDomain
      with SchemeModFCallSiteSensitivity
      with LIFOWorklistAlgorithm[SchemeExp]
    analysis.analyze(timeout())
    val r = analysis.finalResult
    analysis.visited.foreach(println)
    analysis.deps.foreach(println)
    println(r)
  }

  val bench: List[String] = List(
    //"test/R5RS/SETL/setl-benchmarks/arithmetic.scm"
    "test/R5RS/scp1-compressed/all.scm"
  )

  // Used by webviz.
  def newStandardAnalysis(text: String) = {
    val program = SchemeParser.parse(text)
    new SimpleSchemeModFAnalysis(program)
      with SchemeModFNoSensitivity
      with SchemeConstantPropagationDomain
      with DependencyTracking[SchemeExp]
      with FIFOWorklistAlgorithm[SchemeExp] {
      override def intraAnalysis(cmp: SchemeModFComponent): IntraAnalysis with BigStepModFIntra =
        new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
    }
  }

  bench.foreach({ b =>
    try {
      print(b + " => ")
      val t0 = System.currentTimeMillis()
      one(b, () => Timeout.start(Duration(2, MINUTES)))
      val t1 = System.currentTimeMillis()
      println(s"    in ${(t1 - t0)}ms")
    } catch {
      case t: Throwable =>
        println(s"Raised exception.")
        System.err.println(t.getMessage)
        t.printStackTrace() //t.getStackTrace.take(10).foreach(System.err.println)
        System.err.flush()
    }
  })

}
