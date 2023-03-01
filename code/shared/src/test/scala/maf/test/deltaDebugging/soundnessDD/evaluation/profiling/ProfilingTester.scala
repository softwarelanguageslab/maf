package maf.test.deltaDebugging.soundnessDD.evaluation.profiling

import maf.language.scheme.SchemeExp
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.{SoundnessCountingDDTester, SoundnessDDTester}
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

trait ProfilingTester extends SoundnessCountingDDTester:

  def profilingRunAndCompare(program: SchemeExp, benchmark: Benchmark): Option[(String, Array[(String, Int)])] = {
    try
      val concreteResults = evalProgram(program, benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program, benchmark)
      // check if the analysis results soundly (over-)approximate the concrete results
      Some(compareResults(anl, concreteResults),
        anl.asInstanceOf[SequentialWorklistAlgorithm[SchemeExp]].getReAnalysisMap().toArray.sortWith((tpl1, tpl2) => tpl1._2 > tpl2._2))
    catch case exc: Throwable =>
      None
  }

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("Profiling >>> running benchmark: " + benchmark)
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)

    profilingRunAndCompare(program, benchmark) match
      case Some((failureMsg, initAnalysisResults)) =>
        if failureMsg.nonEmpty then
          ProfilingDD.reduce(program, this, benchmark, initAnalysisResults)
      case None =>
