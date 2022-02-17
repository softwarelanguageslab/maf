package maf.cli.runnables

import maf.cli.experiments.SchemeAnalyses
import maf.core.{Identifier, Monad}
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.*
import maf.modular.incremental.ProgramVersionExtracter.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.modular.worklist.*
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls

object AnalyzeProgram extends App:
    def runAnalysis(bench: String, analysis: SchemeExp => ModAnalysis[SchemeExp], timeout: () => Timeout.T): Unit =
        val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
        val a = analysis(text)
        print(s"Analysis of $bench ")
        try {
          val time = Timer.timeOnly {
            a.analyzeWithTimeout(timeout())
            //println(a.program.prettyString())
          }
          println(s"terminated in ${time / 1000000} ms.")
          //a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c).toString()) }).foreach(println)
        } catch {
          case t: Throwable =>
            println(s"raised exception.")
            System.err.println(t.getMessage)
            t.printStackTrace()
            System.err.flush()
        }

    val bench: List[String] = List(
      "test/DEBUG1.scm"
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

    // Used by incremental analyses.
    def newNonIncAnalysis(program: SchemeExp) =
      new ModAnalysis[SchemeExp](program)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsM
        with LIFOWorklistAlgorithm[SchemeExp]
        with BigStepModFSemantics
        with SchemeTypeDomain
        with GlobalStore[SchemeExp] {
        var cnt = 0
        override def run(timeout: Timeout.T) =
            super.run(timeout)
            println(cnt)
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with GlobalStoreIntra {
          override def analyzeWithTimeout(timeout: Timeout.T): Unit =
              cnt = cnt + 1
              super.analyzeWithTimeout(timeout)
        }
      }

    bench.foreach({ b =>
      // for(i <- 1 to 10) {
      //runAnalysis(b, program => SchemeAnalyses.kCFAAnalysis(program, 0), () => Timeout.start(Duration(2, MINUTES)))
      runAnalysis(b, program => newNonIncAnalysis(program), () => Timeout.start(Duration(10, MINUTES)))
      //  }
    })
