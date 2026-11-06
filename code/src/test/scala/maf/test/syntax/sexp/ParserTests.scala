package maf.test.syntax.sexp

import org.scalatest.flatspec.AnyFlatSpec

import maf.syntax.Source
import maf.syntax.sexp.*
import maf.test.*
import maf.util.*

trait SExpParserTestsSpec extends SchemeBenchmarkTests:
    def onBenchmark(benchmark: Benchmark) =
        property(s"SExpParser can correctly parse $benchmark", ParserTest) {
            val content = Reader.loadFile(benchmark)
            val parsed = SExpParser.parse(content, Source.File(benchmark))
            // Check that the parsing was succesful
            assert(parsed.mkString("").nonEmpty)
            // Check that printing and parsing the result again gives the same result
            val printed = parsed.mkString("")
            val reparsed = SExpParser.parse(printed, Source.Text)
            assert(parsed.mkString("") == reparsed.mkString(""), "Printing and parsing again gives a result different from the original parse")
        }

class SExpParserTests extends SExpParserTestsSpec with AllSequentialBenchmarks

class SExpParserSimpleTests extends AnyFlatSpec:
    "File that ends with a comment" should "be parsed without error" in {
        // Issue #8
        val program = "#t ;; foo"
        val parsed = SExpParser.parse(program, Source.Text)
        assert(parsed.size == 1)
        assert(parsed(0).isInstanceOf[SExpValue])
        assert(parsed(0).asInstanceOf[SExpValue].value == Value.Boolean(true))
    }
    "Use of backets or parenthesis" should "not changed the parsed expression" in {
        val program1 = SExpParser.parse("((foo) bar)", Source.Text)
        val program2 = SExpParser.parse("[(foo) bar]", Source.Text)
        val program3 = SExpParser.parse("([foo] bar)", Source.Text)
        // expressions are only equal to themselves, so compare their printed forms instead
        assert(program1.mkString == program2.mkString)
        assert(program1.mkString == program3.mkString)
    }
    "An identifier starting with -" should "be parsed without error" in {
        val program = SExpParser.parse("->", Source.Text)
        assert(program(0).isInstanceOf[SExpId])
        assert(program(0).asInstanceOf[SExpId].id.name == "->")
    }

    "An identifier starting with ~" should "be parsed without error" in {
        val program = SExpParser.parse("~>", Source.Text)
        assert(program(0).isInstanceOf[SExpId])
        assert(program(0).asInstanceOf[SExpId].id.name == "~>")

        val program2 = SExpParser.parse("~>d", Source.Text)
        assert(program2(0).isInstanceOf[SExpId])
        assert(program2(0).asInstanceOf[SExpId].id.name == "~>d")
    }
