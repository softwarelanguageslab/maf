package maf.test.syntax.scheme

import org.scalatest.flatspec.AnyFlatSpec

import maf.syntax.*
import maf.syntax.scheme.*
import maf.test.*
import maf.util.*

trait SchemeParserTestsSpec extends SchemeBenchmarkTests:
    def parseProgram(content: String, source: Source): List[SchemeExp] =
        SchemeParser.parse(content, source)

    def name: String = "SchemeParser"

    def onBenchmark(benchmark: Benchmark) =
        property(s"$name can correctly parse $benchmark", ParserTest) {
            val content = Reader.loadFile(benchmark)
            val parsed = parseProgram(content, Source.File(benchmark))
            // Check that the parsing was succesful
            assert(parsed.nonEmpty)
            // Check that printing and parsing the result again gives the same result
            val printed = parsed.mkString(" ")
            val reparsed = parseProgram(printed, Source.Text)
            assert(parsed.toString == reparsed.toString, "Printing and parsing again gives a result different from the original parse")
            assert(reparsed.flatMap(_.subexpressions).forall(e => e.pos.forall(_.source == Source.Text)))
        }

class SchemeParserTests extends SchemeParserTestsSpec with AllSequentialBenchmarks

class AnnotationTests extends AnyFlatSpec:
    "Annotated lambdas" should "have their annotation accessible" in {
        val program = "(lambda (x) @annot:test x)"
        val parsed = SchemeParser.parse(program, Source.Text).head
        assert(parsed.isInstanceOf[SchemeLambda])
        assert(parsed.asInstanceOf[SchemeLambda].annotation == Some(("@annot", "test")))
    }
