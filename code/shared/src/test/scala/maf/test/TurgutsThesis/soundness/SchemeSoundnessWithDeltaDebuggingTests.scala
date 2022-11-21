package maf.test.TurgutsThesis.soundness

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.{DeleteChildSimple, IfToBegin, RemoveCalls, RemoveCallsAndReplaceByBody, RemoveLambdaParamWithDeepDrop, ReplaceByChild, ReplaceIdentifier, ReplaceIdentifierCalls, TransformationManager}
import maf.TurgutsThesis.gtr.variants.{CountingGTR, FirstInternalGTR, JumpyGTR, SimpleGTR}
import maf.core.{Identity, NoCodeIdentity}
import maf.language.CScheme.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.*
import maf.language.scheme.interpreter.ConcreteValues.*
import maf.language.scheme.lattices.SchemeOp
import maf.language.scheme.primitives.SchemePrelude
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration.{Duration, SECONDS}

trait SchemeSoundnessWithDeltaDebuggingTests extends SchemeSoundnessTests:
  override def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(5, SECONDS))
  override def concreteTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(2, SECONDS)) //remember: concrete run may not halt

  protected def compareResults(
                      analysis: Analysis,
                      concreteResults: Map[Identity, Set[Value]],
                            ): String =
    val analysisResults = analysis.resultsPerIdn
    concreteResults.foreach { case (idn, concreteValues) =>
      val abstractValues = analysisResults.getOrElse(idn, Set.empty)
      concreteValues.foreach { concreteValue =>
        if !abstractValues.exists(checkSubsumption(analysis)(concreteValue, _)) then
          return
            s"""
               | Result at $idn is unsound:
               | - concrete value: $concreteValue
               | - abstract values: ${analysis.lattice.join(abstractValues)}
          """.stripMargin
      }
    }
    ""

  override def evalConcrete(program: SchemeExp, benchmark: Benchmark): Map[Identity, Set[Value]] =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    try
      val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
      for _ <- 1 to times do
        val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
        runInterpreter(interpreter, program, timeout)
    idnResults

  override def runAnalysis(program: SchemeExp, benchmark: Benchmark): Analysis =
    // analyze the program using a ModF analysis
    val anl = analysis(program)
    val timeout = analysisTimeout(benchmark)
    anl.analyzeWithTimeout(timeout)
    assume(anl.finished, "Analysis timed out")
    anl

  override def onBenchmark(benchmark: Benchmark): Unit =
      property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
        // load the benchmark program
        val content = Reader.loadFile(benchmark)
        val program = parseProgram(content, benchmark)

        def runAndCompare(program: SchemeExp): String = {
          try { // run the program using a concrete interpreter
            val concreteResults = evalConcrete(program, benchmark)
            // analyze the program using a ModF analysis
            val anl = runAnalysis(program, benchmark)
            // check if the analysis results soundly (over-)approximate the concrete results
            compareResults(anl, concreteResults)
          }
          catch {
            error => ""
          }
        }

        var failureMsg = runAndCompare(program)
        var count = 0
        if failureMsg.nonEmpty then
          val reduced = GTR.reduce(
            program,
            p => {
              /** without the line below, one might have undefined variables that are never needed dynamically (e.g. dead-code)
               *  And that brings shallow/deep dropping into an infinite loop, since they try to drop all undefined variables
               */
              count += 1
              (p.findUndefinedVariables() equals List()) &&
              runAndCompare(p).nonEmpty //non-empty failure msg
            },
            TransformationManager.allTransformations
          )

          val parsedAgain = SchemeParser.parseProgram(reduced.prettyString()) //parse again, to generate file-related information (e.g. bug is at offset 20-25)
          failureMsg = runAndCompare(parsedAgain)

          fail(
            "FAILED\n: " +
            failureMsg + "\n" +
            "ORIGINAL PROGRAM: \n" +
            program.size + "\n" +
            "REDUCED PROGRAM: \n" +
            reduced.size + "\n" +
            reduced.prettyString()
          )
      }