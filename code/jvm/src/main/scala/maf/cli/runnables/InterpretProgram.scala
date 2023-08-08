package maf.cli.runnables

import maf.language.CScheme.*
import maf.language.change.CodeVersion
import maf.language.change.CodeVersion.*
import maf.language.scheme.interpreter.*
import maf.language.scheme.primitives.SchemePrelude
import maf.util.ColouredFormatting.markOK
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration.*

object InterpretProgram:

    def main(args: Array[String]): Unit =
        val (file, timeout, version) = args.toList match {
            case file :: Nil => (file, 10, New)
            case file :: min :: Nil => (file, min.toInt, New)
            case file :: min :: version :: Nil => (file, min.toInt, CodeVersion.withName(version))
            case _ => throw new Exception("Wrong number of arguments provided.")
        }
        val text = Reader.loadFile(file)
        val interpreter = new SchemeInterpreter((_, _) => ())
        try
            val res = interpreter.run(
              CSchemeParser.parseProgram(text),
              Timeout.start(Duration(timeout, MINUTES)),
              version
            )
            println(res)
            println(markOK("done"))
        catch case ProgramError(msg) => System.err.nn.println(msg)
