package maf.test.modular.scv

import maf.cli.modular.scv._
import maf.test._
import maf.language.ContractScheme._
import maf.language.scheme._
import maf.language.scheme.lattices._
import maf.modular._
import maf.modular.scv._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.util._

/**
 * This type of test checks whether the analysis is able to obtain the expected precision.
 *
 * Precision in this case is measured whether it is able to elliminate program paths that introduce blames but that are actually infeasible at runtime
 */
trait ScvSafetyTests extends ScvAnalysisTests:
    override def onBenchmark(b: Benchmark): Unit =
      property(s"$b should contain no contract violations") {
        runFromFile(b) { an =>
            assert(an.summary.blames.size == 0)
            assert(an.returnValue(an.initialComponent) != an.lattice.bottom)
        }
      }

class SimpleSafetyTests extends ScvSafetyTests with ContractSafetyTestsBenchmarks
