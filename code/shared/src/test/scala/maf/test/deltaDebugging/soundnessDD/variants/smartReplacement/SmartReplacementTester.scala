package maf.test.deltaDebugging.soundnessDD.variants.smartReplacement

import maf.core.Identity
import maf.deltaDebugging.treeDD.transformations.traits.Replacing
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, FileIO, IO, SmartReplaceInterpreter}
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait SmartReplacementTester extends SoundnessDDTester {
  def bugName: String

  def compareResults_(analysis: Analysis, concreteResults: Map[Identity, Set[Value]]): (String, Option[Value]) =
    val analysisResults = analysis.resultsPerIdn
    concreteResults.foreach { case (idn, concreteValues) =>
      val abstractValues = analysisResults.getOrElse(idn, Set.empty)
      concreteValues.foreach { concreteValue =>
        if !abstractValues.exists(checkSubsumption(analysis)(concreteValue, _)) then
          return
            (
            s"""
               | Result at $idn is unsound:
               | - concrete value: $concreteValue
               | - abstract values: ${analysis.lattice.join(abstractValues)}
          """.stripMargin,
              Some(concreteValue)
            )
      }
    }
    ("", None)

  def runAndCompare_(program: SchemeExp, benchmark: Benchmark):
  Option[(String, Map[SchemeExp, Set[ConcreteValues.Value]], Option[Value])] = {
    try
      val concreteResults = replacingEvalProgram(program, benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program, benchmark)
      // check if the analysis results soundly (over-)approximate the concrete results
      val compared = compareResults_(anl, concreteResults._1)

      Some(compared._1, concreteResults._2, compared._2)
    catch case exc: Throwable =>
      None
  }

  override def createInterpreter(addResult: (Identity, Value) => Unit, io: IO, benchmark: Benchmark): SmartReplaceInterpreter =
    new SmartReplaceInterpreter(addResult, io)

  def replacingEvalProgram(program: SchemeExp, benchmark: Benchmark): (Map[Identity, Set[Value]], Map[SchemeExp, Set[Value]]) =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
    var dynAnalysisResults: Map[SchemeExp, Set[ConcreteValues.Value]] = Map()
    for _ <- 1 to times do
      dynAnalysisResults = interpreter.runAndProfile(program, timeout)._2
    (idnResults, dynAnalysisResults)

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("SmartReplacement >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)

    runAndCompare_(program, benchmark) match
      case Some((failureMsg, dynAnalysis, maybeValue)) =>
        if failureMsg.nonEmpty then
          SmartReplacementDD.problematicValue = maybeValue
          SmartReplacementDD.valuesMap = dynAnalysis
          SmartReplacementDD.bugName = bugName
          SmartReplacementDD.reduce(program, this, benchmark)
      case _ =>
}
