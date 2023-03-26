package maf.test.deltaDebugging.soundnessDD.variants.DeadCodeElimination

import maf.core.{Identity, IdentityWithData, NoCodeIdentity, NoCodeIdentityDebug, SimpleIdentity}
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.*
import maf.language.scheme.*
import maf.language.sexp
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.test.deltaDebugging.soundnessDD.variants.baseline.BaselineDD
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timer

trait DeadCodeTester extends SoundnessDDTester {
  def bugName: String

  override def createInterpreter(addResult: (Identity, Value) => Unit, io: IO, benchmark: Benchmark): DeadCodeInterpreter =
    new DeadCodeInterpreter(addResult, io)

  def evalProgramAndFindLambdas(program: SchemeExp, benchmark: Benchmark): (Map[Identity, Set[Value]], Set[SchemeExp]) =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter: DeadCodeInterpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
    var dynAnalysis: Set[SchemeExp] = Set()
    for _ <- 1 to times do
      val (ellapsed, _) = Timer.time({
        val tpl = interpreter.runAndIdentifyDeadCode(program, timeout)
        dynAnalysis = tpl._2
      })
      SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = true)
    (idnResults, dynAnalysis)

  def runAndFindLambdas(program: SchemeExp, benchmark: Benchmark):
  (Option[(String,
    Set[SchemeExp]
    )], (Long, Long)) =
    var evalStartTime: Long = 0
    var evalRunTime: Long = 0
    var evalEndTime: Long = 0

    var analysisStartTime: Long = 0
    var analysisRuntime: Long = 0
    var analysisEndTime: Long = 0

    var excThrown: Boolean = false

    var concreteResults: Option[Map[Identity, Set[Value]]] = None
    var anl: Option[Analysis] = None
    var calledExps: Option[Set[SchemeExp]] = None

    try
      evalStartTime = System.currentTimeMillis()
      val tpl = evalProgramAndFindLambdas(program, benchmark)
      calledExps = Some(tpl._2)
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
    else (Some((compareResults(anl.get, concreteResults.get),
      calledExps.get
    )), (evalRunTime, analysisRuntime))

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("DeadCode >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)

    runAndFindLambdas(program, benchmark) match
      case (Some((failureMsg, dynAnalysis)), _) =>
        if failureMsg.nonEmpty then
          DeadCodeDD.bugName = bugName

          val postDCE = DeadCodeRemover.removeDeadLambdas(program, dynAnalysis, this, benchmark)
          println("pre: " + program.size)
          println("post: " + postDCE.size)
          //println(program.prettyString())
          //println(postDCE.prettyString())
          DeadCodeDD.reduce(program, postDCE, this, benchmark)
      case _ =>
}
