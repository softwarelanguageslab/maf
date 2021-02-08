package maf.cli.runnables

import maf.language.CScheme._
import maf.language.change.CodeVersion._
import maf.language.scheme.interpreter._
import maf.language.scheme.primitives.SchemePrelude
import maf.util.Reader
import maf.util.benchmarks.Timeout

object InterpretProgram extends App {
  val text = Reader.loadFile("test/changes/cscheme/threads/msort.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), stack = true)
  val res = interpreter.run(
    CSchemeUndefiner.undefine(
      List(SchemePrelude.addPrelude(CSchemeParser.parse(text), Set("newline", "display")))
    ),
    Timeout.none,
    New
  )
  println(res)
}
