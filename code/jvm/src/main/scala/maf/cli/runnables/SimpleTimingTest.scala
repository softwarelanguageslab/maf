package maf.cli.runnables

import java.io.File

import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeExp
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.ssmodconc._
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.modular._
import maf.util.Reader
import maf.util.benchmarks._

import scala.concurrent.duration._

object SimpleTimingTest extends App {

  type Analysis = ModAnalysis[SchemeExp] with GlobalStore[SchemeExp]

  def analysis(program: SchemeExp): Analysis =
    new ModAnalysis(program) with KKallocModConc with SchemeConstantPropagationDomain
    with LIFOWorklistAlgorithm[SchemeExp] {
      val k = 1
      override def intraAnalysis(component: SmallStepModConcComponent) =
        new IntraAnalysis(component) with SmallStepIntra with KCFAIntra
    }

  def run(benchmark: String): Unit = {
    System.out.print(benchmark + " ")
    System.out.flush()
    val text = CSchemeParser.parse(Reader.loadFile(benchmark))
    val a    = analysis(text)
    val to   = Timeout.start(Duration(1, MINUTES))
    val time = Timer.timeOnly(a.analyze(to))
    if (to.reached) {
      System.out.println("timed out.")
    } else {
      System.out.println(s"finished in ${time / 1000000}ms.")
    }
  }

  // Kind of warm-up.
  System.err.println("Warm-up")
  System.err.flush()
  SchemeBenchmarks.other.foreach(run)

  // Actual tests.
  // System.err.println("Run")
  // System.err.flush()
  // SchemeBenchmarks.threads.foreach(run)

  // Just copy-paste for this
  object SchemeBenchmarks {

    def files(dir: File): Array[File] = {
      val lst = dir.listFiles()
      if (lst == null) Array()
      else lst
    }

    def fromFolder(directory: String, exclude: String*): Set[String] = {
      val root = new File(directory)
      val base = root.getAbsolutePath.length - directory.length
      files(root).filter(!_.isDirectory).map(_.getAbsolutePath.substring(base)).toSet -- exclude
        .map(file => s"$directory/$file")
    }

    lazy val other: Set[String] = Set(
      "test/R5RS/gambit/peval.scm",
      "test/R5RS/gambit/earley.scm", // list->vector
      "test/R5RS/gambit/scheme.scm",
      "test/R5RS/gambit/sboyer.scm",
      "test/R5RS/gambit/nboyer.scm"
    )

    lazy val threads: Set[String] = fromFolder(
      "test/concurrentScheme/threads",
      "abp.scm",       // Unbound reference: display-recorded.
      "lastzero2.scm", // Uses let*, but should use something like letrec*?
      "phild.scm"      // Unbound reference: bool-top
    )
  }
}
