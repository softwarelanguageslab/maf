package maf.test.language.sexp

import org.scalatest.flatspec.AnyFlatSpec

import maf.language.sexp._
import maf.test._
import maf.util._

trait SExpParserTestsSpec extends SchemeBenchmarkTests {
  def onBenchmark(benchmark: Benchmark) =
    property(s"SExpParser can correctly parse $benchmark", ParserTest) {
      val content = Reader.loadFile(benchmark)
      val parsed = SExpParser.parse(content)
      // Check that the parsing was succesful
      assert(parsed.mkString("").nonEmpty)
      // Check that printing and parsing the result again gives the same result
      val printed = parsed.mkString("")
      val reparsed = SExpParser.parse(printed)
      assert(parsed.mkString("") == reparsed.mkString(""),
        "Printing and parsing again gives a result different from the original parse")
    }
}

class SExpParserTests extends SExpParserTestsSpec with SequentialBenchmarks

class SExpParserSimpleTests extends AnyFlatSpec {
  "File that ends with a comment" should "be parsed without error" in {
    // Issue #8
    val program = "#t ;; foo"
    val parsed = SExpParser.parse(program)
    assert(parsed.size == 1)
    assert(parsed(0).isInstanceOf[SExpValue])
    assert(parsed(0).asInstanceOf[SExpValue].value == ValueBoolean(true))
  }
  "Use of backets or parenthesis" should "not changed the parsed expression" in {
    val program1 = SExpParser.parse("((foo) bar)")
    val program2 = SExpParser.parse("[(foo) bar]")
    val program3 = SExpParser.parse("([foo] bar)")
    assert(program1 == program2)
    assert(program1 == program3)
  }
}
