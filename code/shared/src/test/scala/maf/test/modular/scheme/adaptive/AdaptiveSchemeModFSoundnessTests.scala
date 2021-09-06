package maf.test.modular.scheme.adaptive

import maf.test.modular.scheme._
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.scheme._
import maf.modular.adaptive._
import maf.modular.adaptive.scheme._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.test._

trait AdaptiveSchemeModF extends SchemeModFSoundnessTests { outer =>
  def budget: Int
  def name = s"adaptive analysis (b = $budget)"
  def analysis(program: SchemeExp) =
    new AdaptiveModAnalysis(program)
      with AdaptiveSchemeModFSemantics
      with AdaptiveContextSensitivity
      with AdaptiveKCFA
      with SchemeConstantPropagationDomain
      with FIFOWorklistAlgorithm[SchemeExp]
}

class AdaptiveSchemeModFSoundnessTests extends AdaptiveSchemeModF with AllSequentialBenchmarks:
    def budget = 10
    override def isSlow(b: Benchmark) = true
