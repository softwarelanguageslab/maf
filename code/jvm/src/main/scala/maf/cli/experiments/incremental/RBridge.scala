package maf.cli.experiments.incremental

object RBridge:

    // Script must be a pathname relative to the root of the project.
    def runScript(script: String, arguments: String*): Boolean =
        if !script.endsWith(".R") then return false
        val escapeSpaces = script.replace(" ", "\\ ").nn
        val argString = if arguments.nonEmpty then arguments.map(a => s"\"$a\"").mkString(" ", " ", "") else ""
        import sys.process.*
        s"Rscript --vanilla $escapeSpaces${argString}".! == 0
