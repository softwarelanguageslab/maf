package maf.cli.runnables

import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.interpreter.*
import maf.language.scheme.primitives.SchemePrelude
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration.*

object InterpretProgram extends App:
    val text = Reader.loadFile("test/changes/scheme/slip-1-to-2.scm")
    println(CSchemeParser.parseProgram(text).prettyString())
    val interpreter = new SchemeInterpreter((_, _) => (), stack = true)
    try
        val res = interpreter.run(
          CSchemeParser.parseProgram(text),
          Timeout.start(Duration(10, MINUTES)),
          New
        )
        println(res)
    catch case ProgramError(msg) => System.err.nn.println(msg)
