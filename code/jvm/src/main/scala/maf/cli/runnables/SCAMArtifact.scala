package maf.cli.runnables

import maf.cli.experiments.aam.ScvPerformanceComparison
import maf.cli.experiments.scv.CountFalsePositives
import maf.cli.runnables.scv.ScvRepl

object SCAMArtifact:
    lazy val about: String = s"""
  |Object that gives access to the benchmarks for the SCAM artifact. 
 
  |It is controlled using a set of "commands". The available commands are as follows:
  |${commandsHelp}
  """.stripMargin

    lazy val commands = Map(
      "performance" -> "runs the performance benchmarks",
      "precision" -> "runs the precision benchmarks",
      "repl" -> "runs an interactive repl for testing"
    )

    def commandsHelp: String =
        commands.map { case (command, desc) => f"$command%-15s $desc" }.mkString("\n")

    def printAbout(reason: String): Unit =
        println(reason)
        println()
        println(about)

    def runCommand(cmd: String): Unit = cmd match
        case "performance" =>
            ScvPerformanceComparison.main(Array())
        case "precision" =>
            CountFalsePositives.main(Array())
        case "repl" =>
            ScvRepl.main(Array())

    def main(args: Array[String]): Unit =
        args.foreach(println)
        if (args.size != 1) then printAbout("Invalid number of arguments")
        else
            val cmd = args.head
            if !commands.contains(cmd) then printAbout(s"Invalid command $cmd")
            else runCommand(cmd)
