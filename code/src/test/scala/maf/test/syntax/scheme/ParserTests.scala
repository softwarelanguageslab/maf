package maf.test.syntax.scheme

import org.scalatest.flatspec.AnyFlatSpec

import maf.syntax.*
import maf.syntax.scheme.*
import maf.test.*
import maf.util.*

trait SchemeParserTestsSpec extends SchemeBenchmarkTests:
    def parseProgram(content: String, source: Source): List[SchemeExp[Any]] =
        SchemeParser.parse(content, source)

    def name: String = "SchemeParser"

    def onBenchmark(benchmark: Benchmark) =
        property(s"$name can correctly parse $benchmark", ParserTest) {
            val content = Reader.loadFile(benchmark)
            val parsed = parseProgram(content, Source.File(benchmark))
            // Check that the parsing was succesful
            assert(parsed.nonEmpty)
            // Check that printing and parsing the result again gives the same result (where the printed program may contain reserved identifiers, e.g.,
            // for quasiquotes)
            val printed = parsed.mkString(" ")
            val reparsed = SchemeParser.parseInternal(printed, Source.Text)
            assert(parsed.toString == reparsed.toString, "Printing and parsing again gives a result different from the original parse")
            assert(reparsed.flatMap(_.subexpressions).forall(e => e.pos.forall(_.source == Source.Text)))
        }

class SchemeParserTests extends SchemeParserTestsSpec with AllSequentialBenchmarks

class AnnotationTests extends AnyFlatSpec:
    "Annotated lambdas" should "have their annotation accessible" in {
        val program = "(lambda (x) @annot:test x)"
        val parsed = SchemeParser.parse(program, Source.Text).head
        assert(parsed.isInstanceOf[SchemeLambda[Any]])
        assert(parsed.asInstanceOf[SchemeLambda[Any]].annotation == Some(("@annot", "test")))
    }

class QuasiquoteTests extends AnyFlatSpec:
    def parse(program: String): List[SchemeExp[Any]] = SchemeParser.parse(program, Source.Text)

    "Quasiquoted templates" should "allow unquotes and splices of list elements, and unquoted tails" in {
        assert(parse("`(1 ,x ,@xs . ,y)").nonEmpty)
        assert(parse("`(1 `(2 ,(3 ,x)))").nonEmpty)
    }

    they should "reject a splice that is not a list element" in {
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("`(1 . ,@xs)"))
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("`,@xs"))
    }

    they should "reject unquotes with other than one argument" in {
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("`(1 (unquote x y))"))
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("`(1 (unquote))"))
    }

class VectorLiteralTests extends AnyFlatSpec:
    def parse(program: String): List[SchemeExp[Any]] = SchemeParser.parse(program, Source.Text)

    "Vector literals" should "not be supported, whether they are quoted or not" in {
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("#(1 2 3)"))
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("'#(1 2 3)"))
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("'(1 #(2 3))"))
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("(define x 1) `(1 #(2 ,x))"))
    }

class ReservedIdentifierTests extends AnyFlatSpec:
    def parse(program: String): List[SchemeExp[Any]] = SchemeParser.parse(program, Source.Text)

    "Programs" should "not contain reserved identifiers" in {
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("(define #%cons 1)"))
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("(let ((#%cdr car)) (#%cdr '(1 2)))"))
        assertThrows[SchemeCompiler.SchemeCompilerException](parse("(#%cons 1 2)"))
    }

    they should "be able to use quasiquotes, which refer to reserved identifiers once printed" in {
        val parsed = parse("(define x 1) `(1 ,x)")
        val printed = parsed.mkString(" ")
        assert(printed.contains(s"${SchemeParser.reservedPrefix}cons"))
        assertThrows[SchemeCompiler.SchemeCompilerException](parse(printed))
        assert(SchemeParser.parseInternal(printed, Source.Text).toString == parsed.toString)
    }
