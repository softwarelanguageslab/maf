package maf.test.deltaDebugging.soundnessDD.variants.deadCode

import maf.core.{Identity, IdentityWithData, NoCodeIdentity, NoCodeIdentityDebug, SimpleIdentity}
import maf.language.scheme.{SchemeExp, SchemeLambda}
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, DeadCodeSchemeInterpreter, FileIO, IO, SchemeInterpreter}
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.variants.baseline.BaselineDD
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timer

trait DeadCodeTester extends SoundnessDDTester {
  def bugName: String

  override def createInterpreter(addResult: (Identity, Value) => Unit, io: IO, benchmark: Benchmark): DeadCodeSchemeInterpreter =
    new DeadCodeSchemeInterpreter(addResult, io)

  def evalProgramAndIdentifyDeadCode(program: SchemeExp, benchmark: Benchmark): (Map[Identity, Set[Value]], Set[Int]) =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter: DeadCodeSchemeInterpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
    var calledLambdas: Set[Int] = Set()
    for _ <- 1 to times do
      val (ellapsed, _) = Timer.time({
        val tpl = interpreter.runAndIdentifyCalledLambdas(program, timeout)
        calledLambdas = calledLambdas.union(tpl._2)
      })
      SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = true)
    (idnResults, calledLambdas)

  def runAndIdentifyDeadCode(program: SchemeExp, benchmark: Benchmark):
  (Option[(String, Set[Int])], (Long, Long)) =
    var evalStartTime: Long = 0
    var evalRunTime: Long = 0
    var evalEndTime: Long = 0

    var analysisStartTime: Long = 0
    var analysisRuntime: Long = 0
    var analysisEndTime: Long = 0

    var excThrown: Boolean = false

    var concreteResults: Option[Map[Identity, Set[Value]]] = None
    var anl: Option[Analysis] = None
    var calledLambdas: Option[Set[Int]] = None

    try
      evalStartTime = System.currentTimeMillis()
      val tpl = evalProgramAndIdentifyDeadCode(program, benchmark)
      calledLambdas = Some(tpl._2)
      concreteResults = Some(tpl._1)
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
    else (Some((compareResults(anl.get, concreteResults.get), calledLambdas.get)), (evalRunTime, analysisRuntime))

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("DeadCode >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    var program = parseProgram(content, benchmark)

    runAndIdentifyDeadCode(program, benchmark) match
      case (Some((failureMsg, calledLambdas)), _) =>
        if failureMsg.nonEmpty then
          DeadCodeDD.bugName = bugName

          val maybeRemoved = program.deleteChildren(exp => {
            exp match
              case lambda: SchemeLambda =>
                !calledLambdas.contains(lambda.hashCode())
              case _ => false
          })

          maybeRemoved match
            case Some(removed) =>
              val (maybeFailed, _) = runAndIdentifyDeadCode(removed, benchmark)
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
