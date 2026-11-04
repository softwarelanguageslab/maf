package maf.test.analysis.scheme

import maf.analysis.scheme.*
import maf.domain.scheme.*
import maf.fixpoint.FIFOWorklistAlgorithm
import maf.semantics.scheme.*
import maf.syntax.scheme.*
import maf.test.SchemeR5RSTests
import maf.util.Timeout

import scala.concurrent.duration._

/** Checks the result of each R5RS test program, as computed by a DSS analysis. */
trait SchemeR5RSAnalysisTests extends SchemeR5RSTests:

    type Analysis = SchemeModFLocal & SchemeDomain & SchemeModFLocalSensitivity

    def analysis(prg: SchemeExp): Analysis

    def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def testExpr(program: String, answer: Any): Unit =
        val a = analysis(parseProgram(program))
        val l = a.lattice

        import l.Injector._

        a.analyzeWithTimeout(Timeout.start(Duration(30, SECONDS)))
        // All R5RS tests should terminate, no matter the analysis, because they're so simple.
        assert(a.finished, s"Analysis of $program should finish within the given time bound out.")
        val result = l.join(a.results.getOrElse(a.MainComponent, Set.empty).map(_._1))
        assert(l.subsumes(result, answer), s"Primitive computation test failed on program: $program with result $result (expected $answer).")

class ConstantPropagationDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis(prg: SchemeExp) =
        new SchemeModFLocal(prg) with SchemeConstantPropagationDomain with SchemeModFLocalNoSensitivity with FIFOWorklistAlgorithm[SchemeExp]

class PowersetDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis(prg: SchemeExp) =
        new SchemeModFLocal(prg) with SchemePowersetDomain with SchemeModFLocalNoSensitivity with FIFOWorklistAlgorithm[SchemeExp]

class TypeDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis(prg: SchemeExp) =
        new SchemeModFLocal(prg) with SchemeTypeDomain with SchemeModFLocalNoSensitivity with FIFOWorklistAlgorithm[SchemeExp]
