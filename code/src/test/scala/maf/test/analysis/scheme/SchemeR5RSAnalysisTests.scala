package maf.test.analysis.scheme

import maf.analysis.scheme.*
import maf.domain.scheme.*
import maf.fixpoint.analyze
import maf.fixpoint.worklist.FIFOWorklistAlgorithm
import maf.semantics.scheme.*
import maf.syntax.Source
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.{Preluded, SchemeMutableVarBoxer, SchemePrelude}
import maf.test.SchemeR5RSTests
import maf.util.Timeout

import org.scalatest.Assertion
import scala.concurrent.duration._

/** Checks the result of each R5RS test program, as computed by a DSS analysis. */
trait SchemeR5RSAnalysisTests extends SchemeR5RSTests:

    type Analysis = SchemeModFLocal & SchemeDomain & SchemeModFLocalSensitivity

    def analysis: Analysis

    def parseProgram(txt: String): SchemeExp[Named] & Preluded =
        val parsed = SchemeParser.parse(txt, Source.Text)
        val prelud = SchemePrelude.addPrelude(parsed)
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def testExpr(program: String, answer: Any): Assertion =
        val prg = parseProgram(program)
        val a = analysis
        val l = a.lattice

        import l.Injector._

        val solver = a.analyze(prg, FIFOWorklistAlgorithm, Timeout.start(Duration(30, SECONDS)))
        // All R5RS tests should terminate, no matter the analysis, because they're so simple.
        assert(solver.finished, s"Analysis of $program should finish within the given time bound out.")
        val result = l.join(solver.value(a.ResultDependency(a.initialComponent(prg))).map(_._1))
        assert(l.subsumes(result, answer), s"Primitive computation test failed on program: $program with result $result (expected $answer).")

class ConstantPropagationDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis =
        new SchemeModFLocal with SchemeConstantPropagationDomain with SchemeModFLocalNoSensitivity

class PowersetDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis =
        new SchemeModFLocal with SchemePowersetDomain with SchemeModFLocalNoSensitivity

class TypeDSSR5RSCorrectnessTests extends SchemeR5RSAnalysisTests:
    def analysis =
        new SchemeModFLocal with SchemeTypeDomain with SchemeModFLocalNoSensitivity
