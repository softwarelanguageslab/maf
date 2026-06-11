package maf.cli.experiments.clients.concreteSlicing

import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeParser
import maf.util.benchmarks.Timeout
import scala.concurrent.duration.*
import maf.util.MAFLogger
import maf.util.{Reader, Writer}
import maf.core._
import maf.language.scheme.SchemeLexicalAddresser

object ConcreteSlicerMain: 
    val benchmarks: List[String] = 
        List("test/R5RS/various/slice2.scm")

    def run(mkAnalysis: SchemeExp => ConcreteSlicerDependencies, program: String) = 
        val programText = Reader.loadFile(program)
        val exp = SchemeParser.parseProgram(programText)
        val lexicaladdressedExp = SchemeLexicalAddresser.translateProgram(List(exp)).head
        ConcreteSlicer.runSlicer(lexicaladdressedExp)

    def main(args: Array[String]): Unit =
        MAFLogger.disable()
        benchmarks.map(run(ConcreteSlicerDependencies.createAnalysis, _))
