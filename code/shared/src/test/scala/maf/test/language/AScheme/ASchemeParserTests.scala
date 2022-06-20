package maf.test.language.AScheme

import maf.test.language.scheme.SchemeParserTestsSpec
import maf.language.AScheme.ASchemeParser
import maf.language.scheme.SchemeExp
import maf.core.Position
import maf.bench.scheme.SchemeBenchmarkPrograms

class ASchemeParserTests extends SchemeParserTestsSpec:
    override def name: String = "ASchemeParser"

    override def parseProgram(content: String, tag: Position.PTag = Position.noTag): List[SchemeExp] =
        ASchemeParser.parse(content, tag)

    override def benchmarks: Set[String] = SchemeBenchmarkPrograms.actors
