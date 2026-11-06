package maf.runners

import maf.semantics.scheme.interpreter.*
import maf.syntax.Source
import maf.syntax.scheme.SchemeParser
import maf.util.ColouredFormatting.markOK
import maf.util.{Reader, Timeout}

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
              SchemeParser.parseProgram(text, Source.File(file)),
              Timeout.start(Duration(timeout, MINUTES))
            )
            println(res)
            println(markOK("done"))
        catch case ProgramError(msg) => System.err.nn.println(msg)
