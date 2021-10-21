package maf.test.modular.scheme

import maf.language.scheme._
import maf.modular.scheme.modflocal._
import maf.test._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.language.scheme.primitives.SchemePrelude

trait SchemeModFLocalFSSoundnessTests extends SchemeSoundnessTests:
    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

class SchemeModFLocalFSInsensitiveSoundnessTests extends SchemeModFLocalFSSoundnessTests with VariousSequentialBenchmarks:
    def name = "MODF LOCAL (FS) (context-insensitive)"
    def analysis(prg: SchemeExp) =
      new SchemeModFLocalFS(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalNoSensitivity
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalFSAnalysisResults
    override def isSlow(b: Benchmark): Boolean =
      Set(
        "test/R5RS/various/SICP-compiler.scm", // TIMES OUT
        "test/R5RS/various/four-in-a-row.scm", // TIMES OUT
        // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
        "test/R5RS/various/infinite-1.scm",
        "test/R5RS/various/infinite-2.scm",
        "test/R5RS/various/infinite-3.scm",
      ).contains(b)

class SchemeModFLocalFSCallSiteSensitiveSoundnessTests extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks:
    def name = "MODF LOCAL (FS) call-site sensitive)"
    def analysis(prg: SchemeExp) =
      new SchemeModFLocalFS(prg)
        with SchemeConstantPropagationDomain
        with SchemeModFLocalCallSiteSensitivity(1)
        with FIFOWorklistAlgorithm[SchemeExp]
        with SchemeModFLocalFSAnalysisResults
    override def isSlow(b: String): Boolean = true // don't run this for fast tests
