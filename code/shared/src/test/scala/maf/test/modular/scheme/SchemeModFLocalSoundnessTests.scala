package maf.test.modular.scheme

import maf.language.scheme._
import maf.modular.scheme.modflocal._
import maf.test._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.language.scheme.primitives.SchemePrelude
import maf.core.Position

trait SchemeModFLocalSoundnessTests extends SchemeSoundnessTests:
    override def parseProgram(txt: String, benchmark: String): SchemeExp =
        val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

class SchemeModFLocalTestsA extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks:
    def n = 100
    def name = s"DSS"
    def analysis(prg: SchemeExp) =
        new SchemeModFLocal(prg)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalNoSensitivity
            with FIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFLocalAnalysisResults
    override def isSlow(b: Benchmark): Boolean =
        Set(
          // these time out in the analysis
          "test/R5RS/various/SICP-compiler.scm",
          "test/R5RS/various/mceval.scm", 
          "test/R5RS/various/church-6.scm",
          "test/R5RS/various/church-2-num.scm",
          "test/R5RS/various/four-in-a-row.scm",
          // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
          "test/R5RS/various/infinite-1.scm",
          "test/R5RS/various/infinite-2.scm",
          "test/R5RS/various/infinite-3.scm",
        ).contains(b)