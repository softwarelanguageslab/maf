package maf.cli.runnables

import scala.io.StdIn
import maf.language.AScheme.ASchemeParser

object ASchemeRepl:
    def main(args: Array[String]): Unit =
        val input = StdIn.readLine().trim().nn
        val parsed = ASchemeParser.parse(input)
        println(parsed)
