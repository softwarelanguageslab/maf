package maf.test.TurgutsThesis.soundness.dd

import maf.language.scheme.SchemeExp
import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests

abstract class DeltaDebugger:
  def reduce(program: SchemeExp,
             soundnessTester: SchemeSoundnessWithDeltaDebuggingTests,
             benchmark: String,
             analysisProfiling: Array[(String, Int)]
            ): Unit


