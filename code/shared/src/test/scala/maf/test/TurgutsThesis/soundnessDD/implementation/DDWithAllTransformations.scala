package maf.test.TurgutsThesis.soundnessDD.implementation

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.*
import maf.language.scheme.{SchemeExp, SchemeParser}
import maf.test.TurgutsThesis.soundnessDD.implementation.SoundnessDDTester
import org.scalatest.Assertions.fail

object DDWithAllTransformations:
  def reduce(program: SchemeExp,
             soundnessTester: SoundnessDDTester,
             benchmark: String): Unit =
      var count = 0
      val reduced = GTR.reduce(
        program,
        p => {
          count += 1
          soundnessTester.runAndCompare(p, benchmark) match
            case Some(failureMsg) =>
              p.findUndefinedVariables().isEmpty && failureMsg.nonEmpty
            case None => false
        },
        identity,
        TransformationManager.allTransformations
      )

      val parsedAgain = SchemeParser.parse(reduced.prettyString()).head //parse again, to generate file-related information (e.g. bug is at offset 20-25)
      val failureMsg = soundnessTester.runAndCompare(parsedAgain, benchmark).get

      fail(
        "FAILED:\n " +
          failureMsg + "\n" +
          "ORIGINAL PROGRAM: \n" +
          program.size + "\n" +
          "REDUCED PROGRAM: \n" +
          reduced.size + "\n" +
          reduced.prettyString()
      )