package maf.test.modular.scheme

import maf.language.scheme._
import maf.modular.scheme.modflocal._
import maf.test._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.language.scheme.primitives.SchemePrelude
import maf.core.Position

trait SchemeModFADIGlobalStoreSoundnessTests extends SchemeSoundnessTests:
    override def parseProgram(txt: String, benchmark: String): SchemeExp =
        val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

class SchemeModFADIGlobalStoreSoundnessTestsInsensitive extends SchemeModFADIGlobalStoreSoundnessTests with VariousSequentialBenchmarks:
    def name = "ADI (context-insensitive)"
    def analysis(prg: SchemeExp) =
        new SchemeModFADIGlobalStore(prg)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalNoSensitivity
            with FIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFADIGlobalStoreAnalysisResults {
        override def run(t: maf.util.benchmarks.Timeout.T) = 
           super.run(t)
           println(ctrlDeps) }
    override def isSlow(b: Benchmark): Boolean =
        Set(
          // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
          "test/R5RS/various/infinite-1.scm",
          "test/R5RS/various/infinite-2.scm",
          "test/R5RS/various/infinite-3.scm",
        ).contains(b)

class SchemeModFLocalADIGlobalStoreSoundnessTestsInsensitive extends SchemeModFADIGlobalStoreSoundnessTests with VariousSequentialBenchmarks:
    def name = "ADI (context-insensitive)"
    def analysis(prg: SchemeExp) =
        new SchemeModFADIGlobalStore(prg)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalCallSiteSensitivity(2) //2-CFA
            with FIFOWorklistAlgorithm[SchemeExp]
            with SchemeModFADIGlobalStoreAnalysisResults
    override def isSlow(b: Benchmark): Boolean = 
        true