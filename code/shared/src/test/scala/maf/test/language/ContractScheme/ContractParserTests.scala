package maf.test.language.ContractScheme

import maf.test._
import maf.util._
import maf.language.ContractScheme._
import maf.core._
import maf.bench.scheme.SchemeBenchmarkPrograms

trait ContractParserTestsSpec extends SchemeBenchmarkTests:
    // TODO[small] abstract this away into a ParserTestsSpec that is paramatrized over the parser (also see SchemeParserTestsSpec)
    def onBenchmark(benchmark: Benchmark) =
      property(s"ContractSchemeParser can correctly parse $benchmark", ParserTest) {
        val content = Reader.loadFile(benchmark)
        val parsed = ContractSchemeParser.compile(content)

        // Check that the parsing was succesful
        assert(parsed.toString.nonEmpty)
        // Check that printing and parsing the result again gives the same result
        val printed = parsed.toString
        val reparsed = ContractSchemeParser.compile(printed, Position.newTag("MAF"))
        assert(parsed.toString == reparsed.toString, "Printing and parsing again gives a result different from the original parse")
        assert(reparsed.subexpressions.forall(e => e.idn.pos.tag.contains("MAF") || e.idn == NoCodeIdentity && e.idn.pos.tag.isEmpty))
      }

class ContractParserTests extends ContractParserTestsSpec with ContractBenchmarks
