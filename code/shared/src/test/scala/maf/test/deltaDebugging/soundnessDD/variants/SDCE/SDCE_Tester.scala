package maf.test.deltaDebugging.soundnessDD.variants.SDCE

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, DeadCodeInterpreter, FileIO, PreHaltInterpreter, SDCE_Interpreter, SchemeInterpreter}
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait SDCE_Tester extends SoundnessDDTester {
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
                  anl: Analysis): (Map[Identity, Set[Value]], Set[SchemeExp]) =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val soundnessTest: () => Boolean = () => compareResults_(anl, idnResults)._1.nonEmpty
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    var dynAnalysis: Set[SchemeExp] = Set()
    val interpreter: SchemeInterpreter =
      problematicValue match
        case Some(value) =>
          new SDCE_Interpreter(
            addResult,
            io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")),
            value,
            soundnessTest)
        case _ =>
          new DeadCodeInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")))
    for _ <- 1 to times do
      interpreter match
        case i: DeadCodeInterpreter =>
          dynAnalysis = i.runAndIdentifyLambdas(program, timeout)._2
        case i: SDCE_Interpreter =>
          dynAnalysis = i.runAndIdentifyLambdas(program, timeout)._2
        case _ => throw new Exception()
    (idnResults, dynAnalysis)

  def runAndCompare_(program: SchemeExp, benchmark: Benchmark, problematicValue: Option[Value]):
  Option[(String, Option[Value], Set[SchemeExp])] = {
    try
      val anl = runAnalysis(program, benchmark)
      val concreteResults =
        evalProgram(program,
          benchmark,
          problematicValue,
          anl)
      // analyze the program using a ModF analysis

      // check if the analysis results soundly (over-)approximate the concrete results
      val compared = compareResults_(anl, concreteResults._1)

      Some(compared._1, compared._2, concreteResults._2)
    catch case exc: Throwable =>
      None
  }

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("Transforming >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)

    runAndCompare_(program, benchmark, None) match
      case Some((failureMsg, problematicValue, dynAnalysis)) =>
        if failureMsg.nonEmpty then
          val postDCE = DeadCodeRemover.removeDeadLambdas(program, dynAnalysis, this, benchmark)
          println("pre: " + program.size)
          println("post: " + postDCE.size)
          SDCE_DD.problematicValue = problematicValue
          SDCE_DD.bugName = bugName
          SDCE_DD.reduce(postDCE, this, benchmark)
      case _ =>
}
