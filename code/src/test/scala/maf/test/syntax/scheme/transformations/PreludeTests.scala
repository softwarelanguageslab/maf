package maf.test.syntax.scheme.transformations

import maf.syntax.Source
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.{SchemeMutableVarBoxer, SchemePrelude}
import maf.test.*
import maf.util.Reader

/**
 * Checks that the program transformations that keep the marker `Preluded` indeed keep the definitions of the prelude, i.e., that a program with this
 * marker does not refer to definitions of the prelude that it does not include.
 */
trait PreludeTestsSpec extends SchemeBenchmarkTests:
    def check(step: String, prg: SchemeExp[Any] | List[SchemeExp[Any]]): Unit =
        val missing = SchemePrelude.missingDefinitions(prg)
        assert(missing.isEmpty, s"After $step, the program refers to definitions of the prelude that it does not include: ${missing.mkString(", ")}")

    def onBenchmark(benchmark: Benchmark) =
        property(s"The transformations keep the definitions of the prelude that $benchmark refers to") {
            val parsed = SchemeParser.parse(Reader.loadFile(benchmark), Source.File(benchmark))
            val prelud = SchemePrelude.addPrelude(parsed)
            check("adding the prelude", prelud)
            val boxed = SchemeMutableVarBoxer.transform(prelud)
            check("boxing", boxed)
            val undefined = SchemeParser.undefine(boxed)
            check("undefining", undefined)
            val renamed = SchemeParser.rename(undefined)
            check("renaming", renamed)
        }

class PreludeTests extends PreludeTestsSpec with AllSequentialBenchmarks
