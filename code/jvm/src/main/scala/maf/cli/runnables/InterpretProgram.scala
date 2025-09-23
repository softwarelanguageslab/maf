package maf.cli.runnables

import maf.language.scheme.interpreter.*
import maf.language.scheme._
import maf.language.scheme.primitives.SchemePrelude
import maf.util.ColouredFormatting.markOK
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration.*

object InterpretProgram:

    def main(args: Array[String]): Unit =
        val (file, timeout) = args.toList match {
            case file :: Nil => (file, 10)
            case file :: min :: Nil => (file, min.toInt)
            case _ => throw new Exception("Wrong number of arguments provided.")
        }
        val text = Reader.loadFile(file)
        val interpreter = new SchemeInterpreter((_, _) => ())
        try
            val res = interpreter.run(
              SchemeParser.parseProgram(text),
              Timeout.start(Duration(timeout, MINUTES))
            )
            println(res)
            println(markOK("done"))
        catch case ProgramError(msg) => System.err.nn.println(msg)
