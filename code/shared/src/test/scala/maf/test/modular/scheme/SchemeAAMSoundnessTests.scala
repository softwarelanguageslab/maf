package maf.test.modular.scheme

import maf.language.scheme.*
import maf.modular.scheme.aam.*
import maf.test._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.language.scheme.primitives.SchemePrelude
import maf.core.Position
import maf.util.benchmarks.Timeout
import scala.concurrent.duration._

trait SchemeAAMSoundnessTests extends SchemeSoundnessTests:
    override def parseProgram(txt: String, benchmark: String): SchemeExp =
        val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

class SchemeAAMTests extends SchemeAAMSoundnessTests with VariousSequentialBenchmarks:
    def name = s"AAM"
    def analysis(prg: SchemeExp) = new SchemeAAMAnalysis(prg, 0) with AAMAnalysisResults
    override def isSlow(b: Benchmark): Boolean =
        Set(
          // these time out in the analysis
          "test/R5RS/various/SICP-compiler.scm",
          "test/R5RS/various/mceval.scm",
          "test/R5RS/various/church-6.scm",
          "test/R5RS/various/church-2-num.scm",
          "test/R5RS/various/four-in-a-row.scm",
          "test/R5RS/various/grid.scm",
          "test/R5RS/various/kcfa3.scm",
          "test/R5RS/various/regex.scm",
          "test/R5RS/various/sat.scm",
          "test/R5RS/various/church.scm",
          "test/R5RS/various/widen.scm",
          "test/R5RS/various/gcipd.scm",
          "test/R5RS/various/map.scm",
          "test/R5RS/various/quasiquoting-simple.scm",
          "test/R5RS/various/rsa.scm",
          "test/R5RS/various/primtest.scm",
          "test/R5RS/various/scm2c.scm",
          "test/R5RS/various/foo.scm",
          // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
          "test/R5RS/various/infinite-1.scm",
          "test/R5RS/various/infinite-2.scm",
          "test/R5RS/various/infinite-3.scm",
        ).contains(b)

class SchemeAAMWithGCTests extends SchemeAAMSoundnessTests with VariousSequentialBenchmarks:
    def name = s"AAM (with abstract counting & GC)"
    def analysis(prg: SchemeExp) = new SchemeAAMGCAnalysis(prg, 0) with AAMAnalysisResults
    override def isSlow(b: Benchmark): Boolean =
        Set(
          // these time out in the analysis
          "test/R5RS/various/SICP-compiler.scm",
          "test/R5RS/various/mceval.scm",
          "test/R5RS/various/church-6.scm",
          "test/R5RS/various/church-2-num.scm",
          "test/R5RS/various/four-in-a-row.scm",
          "test/R5RS/various/regex.scm",
          "test/R5RS/various/foo.scm",
          "test/R5RS/various/church.scm",
          "test/R5RS/various/widen.scm",
          "test/R5RS/various/quasiquoting-simple.scm",
          "test/R5RS/various/rsa.scm",
          // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
          "test/R5RS/various/infinite-1.scm",
          "test/R5RS/various/infinite-2.scm",
          "test/R5RS/various/infinite-3.scm",
        ).contains(b)
