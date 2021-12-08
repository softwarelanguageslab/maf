package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.language.scheme._
import maf.modular._
import maf.modular.incremental.ProgramVersionExtracter._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

// null values are used here due to Java interop
import scala.language.unsafeNulls

object AnalyzeProgram extends App:
    def one(bench: String, timeout: () => Timeout.T): Unit =
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
        /* val a = new ModAnalysis(text) with KKallocModConc with SchemeConstantPropagationDomain
    with LIFOWorklistAlgorithm[SchemeExp] {
      val k = 1

      override def intraAnalysis(component: SmallStepModConcComponent) =
        new IntraAnalysis(component) with KCFAIntra
    }*/

        /*
    val analysis = new SimpleSchemeModFAnalysis(text)
      with SchemeConstantPropagationDomain
      with SchemeModFCallSiteSensitivity
      with LIFOWorklistAlgorithm[SchemeExp]
    analysis.analyzeWithTimeout(timeout())
    val r = analysis.finalResult
    analysis.visited.foreach(println)
    analysis.deps.foreach(println)
    println(r)
         */
        val a = nonIncAnalysis(text)
        a.analyzeWithTimeout(timeout())
        a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c).toString()) }).foreach(println)

    val bench: List[String] = List(
      "test/R5RS/various/procedure.scm"
    )

    // Used by webviz.
    def newStandardAnalysis(text: String) =
        val program = SchemeParser.parseProgram(text)
        new SimpleSchemeModFAnalysis(program)
          with SchemeModFNoSensitivity
          with SchemeConstantPropagationDomain
          with DependencyTracking[SchemeExp]
          with FIFOWorklistAlgorithm[SchemeExp] {
          override def intraAnalysis(cmp: SchemeModFComponent) =
            new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
        }

    // Non-inc counterpart to IncrementalRun
    def nonIncAnalysis(program: SchemeExp) =
      new ModAnalysis[SchemeExp](getUpdated(program)) // Select the program version here.
      with StandardSchemeModFComponents with SchemeModFCallSiteSensitivity with SchemeModFSemanticsM with LIFOWorklistAlgorithm[SchemeExp]
      with BigStepModFSemantics with SchemeConstantPropagationDomain with GlobalStore[SchemeExp] with AnalysisLogging[SchemeExp] {
        override def focus(a: Addr): Boolean = !a.toString.toLowerCase().nn.contains("prm")
        override def intraAnalysis(
            cmp: Component
          ) = new IntraAnalysis(cmp) with BigStepModFIntra with GlobalStoreIntra with AnalysisLoggingIntra
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
