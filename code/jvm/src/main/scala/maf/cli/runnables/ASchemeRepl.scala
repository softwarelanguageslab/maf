package maf.cli.runnables

import scala.io.StdIn
import maf.language.AScheme.ASchemeParser
import maf.language.scheme.SchemeBegin
import maf.core.Identity
import maf.modular.scheme.modactor.SimpleSchemeModActorAnalysis
import maf.util.Reader

object ASchemeRepl:
    def main(args: Array[String]): Unit =
        print("> ")
        val input = StdIn.readLine().trim().nn
        if input != ":q" then
            val program = if !input.startsWith(":f") then input else Reader.loadFile(input.split(' ')(1).nn)
            val parsed = ASchemeParser.parse(program)
            val analysis = new SimpleSchemeModActorAnalysis(SchemeBegin(parsed, Identity.none))
            analysis.analyze()
            println(analysis.storeString())
            main(args)
        else ()
