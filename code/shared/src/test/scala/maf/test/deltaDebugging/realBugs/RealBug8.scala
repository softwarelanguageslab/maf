package maf.test.deltaDebugging.realBugs

import maf.language.scheme.SchemeExp
import maf.modular.worklist.{LIFOWorklistAlgorithm, SequentialWorklistAlgorithm}
import maf.util.benchmarks.Timeout

trait RealBug8 extends LIFOWorklistAlgorithm[SchemeExp]:
  override def run(timeout: Timeout.T): Unit =
    while finished && !timeout.reached do step(timeout)

