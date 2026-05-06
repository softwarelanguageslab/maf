package maf.cli.experiments.clients.dynamicSlicing

import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeParser
import maf.util.benchmarks.Timeout
import scala.concurrent.duration.*
import maf.util.MAFLogger
import maf.util.{Reader, Writer}
import maf.core._
import maf.language.scheme.SchemeLexicalAddresser

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
        println("    Direct dependencies: ")
        print("     ")
        n.descendants.map(node => print(node.id + ", "))
        println()
        println("    Transitive dependencies: ")
        print("     ")
        n.reachableStmts.map(node => print(node.id + ", "))
        println()

    def run(mkAnalysis: SchemeExp => DynamicSlicer, program: String) = 
        val programText = Reader.loadFile(program)
        val exp = SchemeParser.parseProgram(programText)
        val lexicaladdressedExp = SchemeLexicalAddresser.translateProgram(List(exp)).head
        val analysis = mkAnalysis(lexicaladdressedExp)

        analysis.analyzeWithTimeout(Timeout.start(30.seconds))
        println("------------------------------")
        println("nodes:")

        implicit val nodeOrdering: Ordering[DynamicNode] = Ordering.by(_.id)
        analysis.nodes.toList.sorted.map(printDynamicNode)


    def main(args: Array[String]): Unit =
        MAFLogger.disable()
        benchmarks.map(run(DynamicSlicer.createAnalysis, _))
        
