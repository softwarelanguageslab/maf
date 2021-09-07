package maf.test.language.scheme

import org.scalatest.flatspec.AnyFlatSpec

import maf.core._
import maf.language.scheme._
import maf.test._
import maf.util._

trait SchemeParserTestsSpec extends SchemeBenchmarkTests:
    def onBenchmark(benchmark: Benchmark) =
      property(s"SchemeParser can correctly parse $benchmark", ParserTest) {
        val content = Reader.loadFile(benchmark)
        val parsed = SchemeParser.parse(content)
        // Check that the parsing was succesful
        assert(parsed.nonEmpty)
        // Check that printing and parsing the result again gives the same result
        val printed = parsed.mkString(" ")
        val reparsed = SchemeParser.parse(printed, Position.newTag("MAF"))
        assert(parsed.toString == reparsed.toString, "Printing and parsing again gives a result different from the original parse")
        assert(reparsed.flatMap(_.subexpressions).forall(e => e.idn.pos.tag.contains("MAF") || e.idn == NoCodeIdentity && e.idn.pos.tag.isEmpty))
      }

class SchemeParserTests extends SchemeParserTestsSpec with AllBenchmarks

class AnnotationTests extends AnyFlatSpec:
    "Annotated lambdas" should "have their annotation accessible" in {
      val program = "(lambda (x) @annot:test x)"
      val parsed = SchemeParser.parse(program).head
      assert(parsed.isInstanceOf[SchemeLambda])
      assert(parsed.asInstanceOf[SchemeLambda].annotation == Some(("@annot", "test")))
    }
