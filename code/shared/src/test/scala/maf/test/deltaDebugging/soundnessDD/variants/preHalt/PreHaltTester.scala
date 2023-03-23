package maf.test.deltaDebugging.soundnessDD.variants.preHalt

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.{ConcreteValues, PreHaltInterpreter, SchemeInterpreter, FileIO}
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait PreHaltTester extends SoundnessDDTester {
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

  def evalProgram(program: SchemeExp,
                  benchmark: Benchmark,
                  problematicValue: Option[Value],
                  anl: Analysis): Map[Identity, Set[Value]] =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val soundnessTest: () => Boolean = () => compareResults_(anl, idnResults)._1.nonEmpty
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter: SchemeInterpreter =
      problematicValue match
        case Some(value) =>
          new PreHaltInterpreter(addResult,
            io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")),
            value,
            soundnessTest)
        case _ =>
          new SchemeInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")))
    for _ <- 1 to times do
      interpreter.run(program, timeout)
    idnResults

  def runAndCompare_(program: SchemeExp, benchmark: Benchmark, problematicValue: Option[Value]):
  Option[(String, Option[Value])] = {
    try
      val anl = runAnalysis(program, benchmark)
      val concreteResults =
        evalProgram(program,
          benchmark,
          problematicValue,
          anl)
      // analyze the program using a ModF analysis

      // check if the analysis results soundly (over-)approximate the concrete results
      val compared = compareResults_(anl, concreteResults)

      Some(compared._1, compared._2)
    catch case exc: Throwable =>
      None
  }

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("Transforming >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)

    runAndCompare_(program, benchmark, None) match
      case Some((failureMsg, problematicValue)) =>
        if failureMsg.nonEmpty then
          PreHaltDD.problematicValue = problematicValue
          PreHaltDD.bugName = bugName
          PreHaltDD.reduce(program, this, benchmark)
      case _ =>
}
