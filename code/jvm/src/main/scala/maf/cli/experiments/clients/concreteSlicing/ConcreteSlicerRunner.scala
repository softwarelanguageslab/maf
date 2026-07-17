package maf.cli.experiments.clients.concreteSlicing

import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeParser
import maf.util.benchmarks.Timeout
import scala.concurrent.duration.*
import maf.util.MAFLogger
import maf.util.{Reader, Writer}
import maf.core._
import maf.language.scheme.SchemeLexicalAddresser

import maf.language.scheme._
import maf.modular.scheme.modflocal._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.language.scheme.primitives.SchemePrelude
import maf.core.Position

object ConcreteSlicerMain: 
    val benchmarks: List[String] = 
        List("test/R5RS/various/slice-app.scm")

    def parseProgram(txt: String, benchmark: String): SchemeExp =
        val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    // def parseProgram(txt: String, benchmark: String): SchemeExp = 
    //     val exp = SchemeParser.parseProgram(programText)
    //     val lexicaladdressedExp = SchemeLexicalAddresser.translateProgram(List(exp)).head
    //     lexicaladdressedExp

    def run(program: String) = 
        val programText = Reader.loadFile(program)
        val programExp = parseProgram(programText, program)
        val res = ConcreteSlicer.runSlicer(programExp)

    def main(args: Array[String]): Unit =
        MAFLogger.disable()
        val prog = args(0)
        run(prog)
