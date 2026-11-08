package maf.test.analysis.scheme

import maf.analysis.scheme.*
import maf.domain.scheme.*
import maf.fixpoint.*
import maf.semantics.scheme.*
import maf.syntax.Source
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.SchemeMutableVarBoxer
import maf.test.*

trait SchemeModFLocalFSSoundnessTests extends SchemeSoundnessTests:
    override def parseProgram(txt: String, benchmark: String): SchemeExp[Named] =
        val parsed = SchemeParser.parse(txt, Source.File(benchmark))
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

class SchemeModFLocalFSInsensitiveSoundnessTests extends SchemeModFLocalFSSoundnessTests with VariousSequentialBenchmarks:
    def name = "MODF LOCAL (FS) (context-insensitive)"
    def analysis(prg: SchemeExp[Named]) =
        new SchemeModFLocalFS(prg) //TODO!
            with SchemeConstantPropagationDomain
            with SchemeModFLocalNoSensitivity
            with FIFOWorklistAlgorithm[SchemeExp[Named]]
            with SchemeModFLocalFSAnalysisResults
    override def isSlow(b: Benchmark): Boolean =
        Set(
          // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
          "test/R5RS/various/infinite-1.scm",
          "test/R5RS/various/infinite-2.scm",
          "test/R5RS/various/infinite-3.scm",
        ).contains(b)

class SchemeModFLocalFSCallSiteSensitiveSoundnessTests extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks:
    def name = "MODF LOCAL (FS) (call-site sensitive)"
    def analysis(prg: SchemeExp[Named]) =
        new SchemeModFLocal(prg) //TODO!
            with SchemeConstantPropagationDomain
            with SchemeModFLocalCallSiteSensitivity(1)
            with FIFOWorklistAlgorithm[SchemeExp[Named]]
            with SchemeModFLocalAnalysisResults
    override def isSlow(b: String): Boolean = true // don't run this for fast tests
