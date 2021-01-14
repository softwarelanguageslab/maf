package maf.cli.runnables

import maf.modular.contracts.ScGlobalStoreAnalysis
import maf.modular.contracts.ScJVMAnalysis
import maf.modular.contracts.SimpleScSemantics
import scala.io.StdIn.readLine
import maf.language.contracts.SCExpCompiler
import maf.modular.contracts.ScCallInsensitivity
import maf.modular.contracts.ScConstantPropagationDomain
import maf.modular.contracts.ScMain
import maf.language.contracts.ScPrelude
import maf.modular.contracts.ScAnalysisWithPrelude

/** A tiny REPL for testing soft contract verification on smaller programs */
object SoftContractConsole extends App {
  def repl(): Unit = {
    print("> ")
    val input = readLine().trim()
    if (input != ":q") {
      val exp =
        try SCExpCompiler.read(input)
        catch {
          case e: Exception =>
            println(s"error while parsing expression ${e.getMessage()}")
            return repl()
        }

      println(s"Analysing ${exp}")

      val analyser = new SimpleScSemantics(exp)
        with ScConstantPropagationDomain
        with ScCallInsensitivity
        with ScJVMAnalysis
        with ScGlobalStoreAnalysis
        with ScAnalysisWithPrelude

      try {
        analyser.analyze()
        println("Analysis terminated")
        println("Summary")
        println(s"Parsed expression $exp")
        println(s"ScMain return value ${analyser.summary.returnValues(ScMain)}")
        println(s"Blames map ${analyser.summary.blames}")
      } catch {
        case e: Exception =>
          println(s"an error occured during the analysis '${e.getMessage()}'")
      }
      repl()
    }
  }

  repl()
}
