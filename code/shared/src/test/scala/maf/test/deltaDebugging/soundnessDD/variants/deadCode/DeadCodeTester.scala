package maf.test.deltaDebugging.soundnessDD.variants.deadCode

import maf.core.{Identity, IdentityWithData, NoCodeIdentity, NoCodeIdentityDebug, SimpleIdentity}
import maf.language.scheme.{SchemeExp, SchemeLambda}
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, FileIO, SchemeInterpreter}
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.variants.baseline.BaselineDD
import maf.test.deltaDebugging.soundnessDD.{SoundnessCountingDDTester, SoundnessDDTester}
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timer

trait DeadCodeTester extends SoundnessCountingDDTester {
  val bugName: String

  def evalProgram(program: SchemeExp, benchmark: Benchmark, maxSteps: Long): (Map[Identity, Set[Value]], Long, Set[Int]) =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
    var calledLambdas: Set[Int] = Set()
    for _ <- 1 to times do
      val (ellapsed, _) = Timer.time({
        val tpl = interpreter.runAndIdentifyCalledLambdas(program, maxSteps)
        calledLambdas = calledLambdas.union(tpl._2)
      })
      SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = true)
    (idnResults, interpreter.getEvalSteps(), calledLambdas)

  def runWithMaxStepsAndIdentifyDeadCode(program: SchemeExp, benchmark: Benchmark, maxSteps: Long):
  (Option[(String, Set[Int], Long)], (Long, Long)) =
    var evalStartTime: Long = 0
    var evalRunTime: Long = 0
    var evalEndTime: Long = 0

    var analysisStartTime: Long = 0
    var analysisRuntime: Long = 0
    var analysisEndTime: Long = 0

    var excThrown: Boolean = false

    var concreteResults: Option[Map[Identity, Set[Value]]] = None
    var evalSteps: Long = 0
    var anl: Option[Analysis] = None
    var calledLambdas: Option[Set[Int]] = None

    try
      evalStartTime = System.currentTimeMillis()
      val tpl = evalProgram(program, benchmark, maxSteps)
      calledLambdas = Some(tpl._3)
      concreteResults = Some(tpl._1)
      evalSteps = tpl._2
      evalEndTime = System.currentTimeMillis()
      evalRunTime = evalEndTime - evalStartTime
    catch case exc: Throwable =>
      excThrown = true
      evalEndTime = System.currentTimeMillis()
      evalRunTime = evalEndTime - evalStartTime

    try
      analysisStartTime = System.currentTimeMillis()
      anl = Some(runAnalysis(program, benchmark))
      analysisEndTime = System.currentTimeMillis()
      analysisRuntime = analysisEndTime - analysisStartTime
    catch case exc: Throwable =>
      excThrown = true
      analysisEndTime = System.currentTimeMillis()
      analysisRuntime = analysisEndTime - analysisStartTime

    if excThrown then
      (None, (evalRunTime, analysisRuntime))
    else (Some((compareResults(anl.get, concreteResults.get), calledLambdas.get, evalSteps)), (evalRunTime, analysisRuntime))

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("DeadCode >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    var program = parseProgram(content, benchmark)

    runWithMaxStepsAndIdentifyDeadCode(program, benchmark, Long.MaxValue) match
      case (Some((failureMsg, calledLambdas, evalSteps)), _) =>
        if failureMsg.nonEmpty then
          DeadCodeDD.maxSteps = evalSteps
          DeadCodeDD.bugName = bugName

          val maybeRemoved = program.deleteChildren(exp => {
            exp match
              case lambda: SchemeLambda =>
                !calledLambdas.contains(lambda.hashCode())
              case _ => false
          })

          maybeRemoved match
            case Some(removed) =>
              val (maybeFailed, _) = runWithMaxStepsAndIdentifyDeadCode(removed, benchmark, evalSteps)
              maybeFailed match
                case Some(tpl) =>
                  if tpl._1.nonEmpty then
                    DeadCodeDD.reduce(program, removed, this, benchmark)
                    return
                case _ =>
            case _ =>


          DeadCodeDD.reduce(program, program, this, benchmark)
      case _ =>
}
