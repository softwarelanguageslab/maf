package maf.runners

import maf.analysis.scheme.*
import maf.domain.*
import maf.domain.lattice.*
import maf.syntax.scheme.*
import maf.util.Reader

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

    lazy val grid = parse(Reader.loadFile("test/R5RS/various/grid.scm"))

    def parse(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def runDSS(prg: SchemeExp) =
        val anl = new SchemeDSSAnalysis(prg, 0) //with NameBasedAllocator
        anl.analyze()
        val res = anl.results(anl.MainComponent)
        val vlu = Lattice[anl.Val].join(res.map(_._1))
        vlu
