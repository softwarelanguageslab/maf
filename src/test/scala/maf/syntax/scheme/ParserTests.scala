package maf.syntax
package scheme

import maf.test.*
import maf.util.*

import org.scalatest.flatspec.AnyFlatSpec

trait SchemeParserTestsSpec extends SchemeBenchmarkTests:
  def parseProgram(
      content: String,
      tag: Option[PTag] = None
  ): List[SchemeExp] =
    SchemeParser.parse(content, tag)

  def name: String = "SchemeParser"

  def onBenchmark(benchmark: Benchmark) =
    property(s"$name can correctly parse $benchmark", ParserTest) {
      val content = Reader.loadFile(benchmark)
      val parsed = parseProgram(content)
      // Check that the parsing was succesful
      assert(parsed.nonEmpty)
      // Check that printing and parsing the result again gives the same result
      val printed = parsed.mkString(" ")
      val reparsed = parseProgram(printed, Some("MAF"))
      assert(
        parsed.toString == reparsed.toString,
        "Printing and parsing again gives a result different from the original parse"
      )
    }

class SchemeParserTests extends SchemeParserTestsSpec with AllBenchmarks

class AnnotationTests extends AnyFlatSpec:
  "Annotated lambdas" should "have their annotation accessible" in {
    val program = "(lambda (x) @annot:test x)"
    val parsed = SchemeParser.parse(program).head
    assert(parsed.isInstanceOf[SchemeLambda])
    assert(
      parsed.asInstanceOf[SchemeLambda].annotation == Some(("@annot", "test"))
    )
  }
