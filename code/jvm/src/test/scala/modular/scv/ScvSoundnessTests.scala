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
 * Soft contract verification is regarded as sound if it generates contract violations for contracts that are violated (unsatisfied) at runtime.
 *
 * This set of tests are manual tests which are specifically designed to detect potential soundness issues in the analysis
 */
trait ScvSoundnessTests extends ScvAnalysisTests:
    def onBenchmark(b: Benchmark): Unit =
      property(s"$b should contain contract violations") {
        runFromFile(b) { an =>
          // TODO: this is not very precise and does not check where the actual error should be
          assert(an.summary.blames.size > 0)
        }
      }

// class ScvManualSoundnessTests extends ContractSoundnessTestsBenchmarks with ScvSoundnessTests
