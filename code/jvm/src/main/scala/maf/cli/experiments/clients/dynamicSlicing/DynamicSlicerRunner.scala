package maf.cli.experiments.clients.dynamicSlicing

import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeParser
import maf.util.benchmarks.Timeout
import scala.concurrent.duration.*
import maf.util.MAFLogger
import maf.util.{Reader, Writer}
import maf.core._

object DynamicSlicerMain: 
    val benchmarks: List[String] = 
        List("test/R5RS/various/slice.scm")

    def printDynamicNode(n: DynamicNode) = 
        println("  ---  ")
        print("  ")
        print(n.id) 
        println(":")
        println("    Expression: ")
        print("     ")
        println(n.exp) 
        println("    Reachable statements: ")
        print("     ")
        n.reachableStmts.map(node => print(node.id + ", "))
        println()
        println("    Descendants: ")
        print("     ")
        n.descendants.map(node => print(node.id + ", "))
        println()

    def printDefnNode(variable: Identifier, n: DynamicNode) = 
        println("  " + variable + ": " + n.id)



    def run(mkAnalysis: SchemeExp => DynamicSlicer, program: String) = 
        val programText = Reader.loadFile(program)
        val exp = SchemeParser.parseProgram(programText)
        val analysis = mkAnalysis(exp)

        analysis.analyzeWithTimeout(Timeout.start(30.seconds))
        println("defnNode:")
        analysis.defnNode.map((k, v) => printDefnNode(k, v))
        println("------------------------------")
        println("nodes:")
        analysis.nodes.map(printDynamicNode)


    def main(args: Array[String]): Unit =
        MAFLogger.disable()
        benchmarks.map(run(DynamicSlicer.createAnalysis, _))
        
