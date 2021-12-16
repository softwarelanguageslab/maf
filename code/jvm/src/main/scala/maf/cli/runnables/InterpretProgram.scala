package maf.cli.runnables

import maf.language.CScheme._
import maf.language.change.CodeVersion._
import maf.language.scheme.interpreter._
import maf.language.scheme.primitives.SchemePrelude
import maf.util.Reader
import maf.util.benchmarks.Timeout

object InterpretProgram extends App:
    val text = Reader.loadFile("test/R5RS/various/procedure.scm")
    val interpreter = new SchemeInterpreter((_, _) => (), stack = true)
    val res = interpreter.run(
      CSchemeParser.parseProgram(text),
      Timeout.none,
      New
    )
    println(res)
