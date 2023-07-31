package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeExp
import maf.modular.ReturnAddr
import maf.modular.adaptive.AdaptiveModAnalysis
import maf.modular.adaptive.scheme._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.util.Reader
import maf.util.benchmarks.Timeout
import maf.modular.scheme.modflocal._
import maf.modular.scheme.aam.*

import scala.concurrent.duration._
import scala.language.reflectiveCalls

import maf.cli.experiments.SchemeAnalyses
import maf.language.scheme.interpreter._
import maf.language.scheme.SchemeMutableVarBoxer
import maf.language.scheme.primitives.SchemePrelude
import maf.language.CScheme.CSchemeLexicalAddresser
import maf.core._

object AdaptiveRun:

    def main(args: Array[String]): Unit = print(runDSS(prg2))

    val prg1 = parse(
        """
        | (define (foo x)
        |    (+ x 1))
        | (define x 5)
        | (foo 1)
        | x
        """.stripMargin
    )

    val prg2 = parse(
        """
        | (let ((a 0))
        |   (set! a (+ a 1))
        |   a)
        """.stripMargin
    )

    val counterExample = parse(
        """
        | (let*
        |   ((g (lambda (v) (lambda () v)))
        |    (v 0)
        |    (inc (lambda (x) (+ x 1)))
        |    (a (g (inc v))))
        |   (a))
        """.stripMargin
    )

    val counterExample2 = parse(
        """
        | (let* ((alloc (lambda (a) (lambda () a)))
        |        (f (lambda (a) (let ((c (alloc 1)) (ignore a)) c)))
        |        (b (f 0)))
        |    (b))
        """.stripMargin
    )

    lazy val grid = parse(Reader.loadFile("test/R5RS/various/grid.scm"))

    def parse(txt: String): SchemeExp =
        val parsed = CSchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        CSchemeParser.undefine(transf)

    def runDSS(prg: SchemeExp) =
        val anl = new SchemeDSSAnalysis(prg, 0) with LoggingEval //with NameBasedAllocator
        anl.analyze()
        val res = anl.results(anl.MainComponent).asInstanceOf[Set[(anl.Val, anl.Dlt, Set[anl.Adr])]]
        val vlu = Lattice[anl.Val].join(res.map(_._1))
        vlu 

    def runAAM(prg: SchemeExp) = 
        val aam = new SchemeAAMAnalysis(prg, 0) with AAMGC with AAMNameBasedAllocator with AAMAnalysisResults
        aam.analyze()
        aam.finalValue