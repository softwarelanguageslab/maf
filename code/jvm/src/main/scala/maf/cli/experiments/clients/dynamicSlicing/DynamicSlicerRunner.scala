package maf.cli.experiments.clients.dynamicSlicing

import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeParser
import maf.util.benchmarks.Timeout
import scala.concurrent.duration.*
import maf.util.MAFLogger

object DynamicSlicerMain: 
    val benchmarks: List[String] = 
        List("test/R5RS/various/slice.scm")

    def run(mkAnalysis: SchemeExp => DynamicSlicer, program: String) = 
        val exp = SchemeParser.parseProgram(program)
        val analysis = mkAnalysis(exp)

        analysis.analyzeWithTimeout(Timeout.start(30.seconds))
        println("defnNode:")
        println(analysis.defnNode)
        println("nodes:")
        println(analysis.nodes)


    def main(args: Array[String]): Unit =
        MAFLogger.disable()
        benchmarks.map(run(DynamicSlicer.createAnalysis, _))
        
