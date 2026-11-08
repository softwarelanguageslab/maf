package maf.test.analysis.scheme

import maf.analysis.scheme.*
import maf.domain.scheme.*
import maf.fixpoint.*
import maf.runners.*
import maf.semantics.scheme.*
import maf.syntax.Source
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.SchemeMutableVarBoxer
import maf.test.*

trait SchemeModFLocalSoundnessTests extends SchemeSoundnessTests:
    override def parseProgram(txt: String, benchmark: String): SchemeExp[Any] =
        val parsed = SchemeParser.parse(txt, Source.File(benchmark))
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

class SchemeModFLocalTestsA extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks:
    def name = s"DSS"
    def analysis(prg: SchemeExp[Any]) =
        new SchemeModFLocal(prg)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalNoSensitivity
            with FIFOWorklistAlgorithm[SchemeExp[Any]]
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

class SchemeModFADITests extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks:

    def name = s"ADI (with GC)"
    def analysis(prg: SchemeExp[Any]) = SchemeAnalyses.modfADIAnalysis(prg, 0)
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
