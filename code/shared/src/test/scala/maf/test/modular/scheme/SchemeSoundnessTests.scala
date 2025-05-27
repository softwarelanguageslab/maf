package maf.test.modular.scheme

import java.util.concurrent.TimeoutException
import org.scalatest.Tag
import maf.core._
import maf.language.CScheme._
import maf.language.scheme.interpreter.ConcreteValues._
import maf.language.scheme._
import maf.language.scheme.interpreter._
import maf.language.scheme.lattices.SchemeOp
import maf.language.scheme.primitives.SchemePrelude
import maf.modular._
import maf.modular.scheme._
import maf.test._
import maf.util._
import maf.util.benchmarks.{Timeout, Timer}

import scala.concurrent.duration._

object SchemeSoundnessTests:
    /** Open a timestamped writer */
    private val writer: Writer.Writer = Writer.openTimeStamped("out/soundness-tests-timing.csv")

    /** Log the time ellapsed for running the analysis */
    def logEllapsed(test: SchemeSoundnessTests, benchmark: String, ellapsed: Long, concrete: Boolean): Unit =
        Writer.write(writer, test.getClass.toString + ";" + benchmark + ";" + ellapsed.toString + ";" + (if concrete then "yes" else "no"))

trait SchemeSoundnessTests extends SchemeBenchmarkTests:
    // analysis must support basic Scheme semantics
    type Analysis = ModAnalysis[SchemeExp] with AnalysisResults[SchemeExp] with SchemeDomain
    // the analysis that is used to analyse the programs
    def name: String
    def analysis(b: SchemeExp): Analysis
    // the timeout and max number of concrete runs for a single benchmark program (default: 1min.)
    def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(1, MINUTES))
    def concreteTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(1, MINUTES))
    def concreteRuns(b: Benchmark): Int = 1 // for highly non-deterministic programs, a higher value is recommended

    /**
     * Create a concrete interpreter
     *
     * @param addResult
     *   a callback function invoked by the interpreter at a store write
     * @param io
     *   a mock IO that is used as input to the programs in the concrete interpreter
     * @return
     *   a concrete SchemeInterpreter
     */
    def createInterpreter(addResult: (Identity, ConcreteValues.Value) => Unit, io: IO = new EmptyIO(), benchmark: String): SchemeInterpreter =
        new SchemeInterpreter(addResult, io)

    /**
     * Hook that gets executed when the interpreter throws an exception.
     *
     * It rethrows the exception as a default behaviour
     *
     * @param addResult
     *   a callback function that can register values in the result map
     */
    def handleInterpreterError(addResult: (Identity, ConcreteValues.Value) => Unit): PartialFunction[Throwable, Any] = { case e => throw e }

    // the actual testing code
    protected def runInterpreter(
        i: SchemeInterpreter,
        p: SchemeExp,
        t: Timeout.T
      ): Value =
        i.run(p, t) // If there are code changes in the file, runs the "new" version by default (ensures compatibility with files containing changes).
    protected def evalConcrete(program: SchemeExp, benchmark: Benchmark): Map[Identity, Set[Value]] =
        var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
        val timeout = concreteTimeout(benchmark)
        val times = concreteRuns(benchmark)
        try
            val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
            for _ <- 1 to times do
                val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
                try
                    val (ellapsed, _) = Timer.time(runInterpreter(interpreter, program, timeout))
                    SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = true)
                catch handleInterpreterError(addResult)
        catch
            case _: TimeoutException =>
                alert(s"Concrete evaluation of $benchmark timed out.")
            case ProgramError(msg) =>
                alert(s"Concrete evaluation of $benchmark encountered a program error:\n$msg")
            case ChildThreadDiedException(_) =>
                alert(s"Concrete evaluation of $benchmark aborted due to a fatal crash in a child thread.")
            case e: VirtualMachineError =>
                System.gc()
                alert(s"Concrete evaluation of $benchmark failed with $e")
        idnResults
    protected def runAnalysis(program: SchemeExp, benchmark: Benchmark): Analysis =
        try
            // analyze the program using a ModF analysis
            val anl = analysis(program)
            val (ellapsed, timeout) = Timer.time(analysisTimeout(benchmark))
            SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = false)
            anl.analyzeWithTimeout(timeout)
            assume(anl.finished, "Analysis timed out")
            anl
        catch
            case e: VirtualMachineError =>
                System.gc()
                cancel(s"Analysis of $benchmark encountered an error: $e")

    protected def checkSubsumption(analysis: Analysis)(v: Value, abs: analysis.Value): Boolean =
        val lat = analysis.lattice
        v match
            case Value.Undefined(_)  => true
            case Value.Void          => lat.subsumes(abs, lat.void)
            case Value.Clo(lam, _)   => lat.getClosures(abs).exists(_._1.idn == lam.idn)
            case Value.Primitive(p)  => lat.getPrimitives(abs).exists(_ == p)
            case Value.Str(s)        => lat.subsumes(abs, lat.string(s))
            case Value.Symbol(s)     => lat.subsumes(abs, lat.symbol(s))
            case Value.Integer(i)    => lat.subsumes(abs, lat.number(i))
            case Value.Real(r)       => lat.subsumes(abs, lat.real(r))
            case Value.Bool(b)       => lat.subsumes(abs, lat.bool(b))
            case Value.Character(c)  => lat.subsumes(abs, lat.char(c))
            case Value.Nil           => lat.subsumes(abs, lat.nil)
            case Value.Pointer(a)    => lat.getPointerAddresses(abs).exists(_.idn == a._2.idn)
            case Value.Thread(_)     => lat.getThreads(abs).nonEmpty
            case Value.Lock(l)       => lat.isTrue(lat.op(SchemeOp.IsLock)(List(abs)).getOrElse(lat.bottom))
            case Value.InputPort(h)  => lat.subsumes(abs, lat.op(SchemeOp.MakeInputPort)(List(lat.string(h.abstractName))).getOrElse(lat.bottom))
            case Value.OutputPort(h) => lat.subsumes(abs, lat.op(SchemeOp.MakeOutputPort)(List(lat.string(h.abstractName))).getOrElse(lat.bottom))
            case Value.EOF           => lat.subsumes(abs, lat.charTop)
            case Value.Cons(a, d) =>
                checkSubsumption(analysis)(a, lat.op(SchemeOp.Car)(List(abs)).getOrElse(lat.bottom)) &&
                    checkSubsumption(analysis)(d, lat.op(SchemeOp.Cdr)(List(abs)).getOrElse(lat.bottom))
            case Value.Vector(siz, els, ini) =>
                    lat.subsumes(lat.op(SchemeOp.VectorLength)(List(abs)).getOrElse(lat.bottom), lat.number(siz)) 
                    && 
                    els.forall { case (idx, vlu) =>
                        checkSubsumption(analysis)(vlu, lat.op(SchemeOp.VectorRef)(List(abs, lat.number(idx))).getOrElse(lat.bottom))
                    } 
                    &&
                    (els.size == siz || checkSubsumption(analysis)(ini, lat.op(SchemeOp.VectorRef)(List(abs, lat.numTop)).getOrElse(lat.bottom)))
            case v => throw new Exception(s"Unknown concrete value type: $v.")

    protected def compareResults(
        analysis: Analysis,
        concreteResults: Map[Identity, Set[Value]],
        message: String = ""
      ): Unit =
        val analysisResults = analysis.resultsPerIdn
        concreteResults.foreach { case (idn, concreteValues) =>
            val abstractValues = analysisResults.getOrElse(idn, Set.empty)
            concreteValues.foreach { concreteValue =>
                if !abstractValues.exists(checkSubsumption(analysis)(concreteValue, _)) then
                    println(concreteValues)
                    println(abstractValues)
                    val failureMsg =
                        s"""
            | Result at $idn is unsound:
            | - concrete value: $concreteValue
            | - abstract values: ${analysis.lattice.join(abstractValues)}
            """.stripMargin
                    if message.isEmpty then fail(failureMsg)
                    else fail(s"$message > $failureMsg")
            }
        }

    // indicate if a benchmark is slow or not
    def isSlow(b: Benchmark) = false

    def testTags(b: Benchmark): Seq[Tag] =
        if isSlow(b) then Seq(SoundnessTest, SlowTest)
        else Seq(SoundnessTest)

    def parseProgram(txt: String, benchmark: String): SchemeExp =
        CSchemeParser.parseProgram(txt, Position.withSourcePath(benchmark))

    def onBenchmark(benchmark: Benchmark): Unit =
        property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
            // load the benchmark program
            val content = Reader.loadFile(benchmark)
            val program = parseProgram(content, benchmark)
            // run the program using a concrete interpreter
            val concreteResults = evalConcrete(program, benchmark)
            // analyze the program using a ModF analysis
            val anl = runAnalysis(program, benchmark)
            // check if the analysis results soundly (over-)approximate the concrete results
            compareResults(anl, concreteResults)
        }
