package maf.cli.runnables

import maf.language.CScheme._
import maf.language.change.CodeVersion.New
import maf.language.scheme.SchemeInterpreter
import maf.language.scheme.primitives.SchemePrelude
import maf.util.Reader
import maf.util.benchmarks.Timeout

object InterpretProgram extends App {
  val text = Reader.loadFile("test/DEBUG1.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), true, true)
  val res = interpreter.run(
    CSchemeUndefiner.undefine(
      List(SchemePrelude.addPrelude(CSchemeParser.parse(text), Set("newline", "display")))
    ),
    Timeout.none,
    New
  )
  println(res)
}
