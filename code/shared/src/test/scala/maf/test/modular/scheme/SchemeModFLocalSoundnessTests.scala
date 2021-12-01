package maf.test.modular.scheme

import maf.language.scheme._
import maf.modular.scheme.modflocal._
import maf.test._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.language.scheme.primitives.SchemePrelude

trait SchemeModFLocalSoundnessTests extends SchemeSoundnessTests:
    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

class SchemeModFLocalAdaptiveTestsA extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks:
    def n = 100
    def name = s"MODF LOCAL w/ ASW -- policy A (n = $n)"
    def analysis(prg: SchemeExp) =
      new SchemeModFLocal(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalNoSensitivity
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalAnalysisResults
        with SchemeModFLocalAdaptiveWideningPolicyA(n)
    override def isSlow(b: Benchmark): Boolean =
      Set(
        "test/R5RS/various/SICP-compiler.scm", // TIMES OUT
        "test/R5RS/various/mceval.scm", // TIMES OUT
        // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
        "test/R5RS/various/infinite-1.scm",
        "test/R5RS/various/infinite-2.scm",
        "test/R5RS/various/infinite-3.scm",
      ).contains(b)

class SchemeModFLocalAdaptiveTestsB extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks:
    def l = 10
    def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
    def analysis(prg: SchemeExp) =
      new SchemeModFLocal(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalNoSensitivity
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalAnalysisResults
        with SchemeModFLocalAdaptiveWideningPolicyB(l)
    override def isSlow(b: Benchmark): Boolean =
      Set(
        "test/R5RS/various/SICP-compiler.scm", // TIMES OUT
        "test/R5RS/various/mceval.scm", // TIMES OUT
        // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
        "test/R5RS/various/infinite-1.scm",
        "test/R5RS/various/infinite-2.scm",
        "test/R5RS/various/infinite-3.scm",
      ).contains(b)
