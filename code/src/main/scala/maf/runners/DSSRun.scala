package maf.runners

import maf.analysis.scheme.*
import maf.domain.*
import maf.domain.lattice.*
import maf.syntax.Source
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.{Preluded, SchemeMutableVarBoxer, SchemePrelude}

object DSSRun:

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

    lazy val grid = prepare(SchemeParser.parseFile("test/R5RS/various/grid.scm"))

    def parse(txt: String): SchemeExp[Named] & Preluded = prepare(SchemeParser.parse(txt, Source.Text))

    def prepare(parsed: List[SchemeExp[Surface]]): SchemeExp[Named] & Preluded =
        val prelud = SchemePrelude.addPrelude(parsed)
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def runDSS(prg: SchemeExp[Named] & Preluded) =
        val anl = new SchemeDSSAnalysis(prg, 0) //with NameBasedAllocator
        anl.analyze()
        val res = anl.results(anl.MainComponent)
        val vlu = Lattice[anl.Val].join(res.map(_._1))
        vlu
