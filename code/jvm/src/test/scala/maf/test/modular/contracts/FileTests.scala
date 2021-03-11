package maf.test.modular.contracts

import maf.language.contracts.ScExp
import maf.test.ScTestsJVM

import scala.io.Source

trait FileTests extends ScTestsJVM {
  def fromFile(filename: String): String = {
    val source = Source.fromFile(filename)
    val code   = source.getLines.mkString("\n")
    source.close()
    code
  }

  def evalFromFile(filename: String): VerifyTestBuilder = {
    SomeVerifyTestBuilder(fromFile(filename))
  }

  def _evalFromFile(_filename: String): VerifyTestBuilder = {
    EmptyVerifyTestBuilder
  }
}
