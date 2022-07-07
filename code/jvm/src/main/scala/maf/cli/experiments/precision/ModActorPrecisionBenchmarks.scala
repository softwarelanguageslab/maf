package maf.cli.experiments.precision

import maf.language.AScheme.ASchemeParser
import maf.modular.scheme.modactor.SchemeModActorSemantics
import maf.language.scheme.*
import maf.core.Lattice
import maf.cli.experiments.SchemeAnalyses
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.Reader
import maf.language.AScheme.interpreter.ConcreteComponentsConversion
import maf.util.MonoidImplicits.FoldMapExtension
import maf.util.benchmarks.Timeout
import concurrent.duration.DurationInt
import java.util.concurrent.TimeoutException
import maf.modular.scheme.modactor.SimpleSchemeModActorAnalysis
import maf.util.benchmarks.Table.apply
import maf.util.Writer
import maf.util.benchmarks.Table
import maf.util.datastructures.MapOps.*

/**
 * Compare a number of concrete runs of the program with the output of the static analyser.
 *
 * We count the number of observed elements that are less precise than the concrete run, we call these elements spurious elements, because they do not
 * correspond with a concrete run of the program and are therefore imprecise.
 */
trait ModActorPrecisionBenchmarks extends ConcreteComponentsConversion:
    enum PrecisionResult:
        case TimedOut
        case Fail
        case Result(imprecise: ObsSpu)

    type Analysis = SimpleSchemeModActorAnalysis

    case class ObsSpu(observed: Int, spurious: Int):
        def +(that: ObsSpu): ObsSpu =
            ObsSpu(this.observed + that.observed, this.spurious + that.spurious)

    implicit def toObsSpu(t: (Int, Int)): ObsSpu = ObsSpu(t._1, t._2)

    /** Specifies the timeout for the static analysis */
    def timeout: Timeout.T = Timeout.start(30.seconds)

    /**
     * Creates a static analysis for the given program
     *
     * @param program
     *   the program to analyse
     */
    def analysis(program: SchemeExp): Analysis

    /** The set of benchmark programs to use for the precision tests */
    def benchmarks: Set[String]

    /** The number of concrete runs to make before joining the runs together */
    def concreteRuns: Int = 10

    protected def alertMsg(msg: String): Unit = ()

    protected def parseProgram(source: String): SchemeExp =
        ASchemeParser.parseProgram(source)

    protected def compareMailbox(anl: Analysis, concreteState: ConcreteState): ObsSpu =
        val abstractMailboxesLifted = anl.getMailboxes.map { case (actor, mailbox) =>
            val newActor = anl.view(actor).removeContext.removeEnv
            val newMailbox = mailbox.messages.map(toMsg(anl))
            newActor -> newMailbox
        }.toMapJoined

        val concreteMailboxesLifted = concreteState.mailboxes
            .map { case (actor, mailbox) =>
                val newActor = concreteToAbstractActor(actor).removeEnv
                val newMailbox = mailbox.map(_.mapValues(convertConcreteValue(anl, _))).toSet
                newActor -> newMailbox
            }
            .toMapJoined
            .withDefaultValue(Set())

        val spurious = abstractMailboxesLifted.map { case (actor, mailbox) =>
            (mailbox -- concreteMailboxesLifted(actor)).size
        }.sum

        val observed = concreteMailboxesLifted.map(_._2.size).sum
        (observed, spurious)

    protected def compareSpawned(anl: Analysis, concreteState: ConcreteState): ObsSpu =
        val abstractSpawnedLifted = anl.getMailboxes.keys.map(_.removeContext.removeEnv).toSet
        val concreteSpawnedLifted = concreteState.mailboxes.keys.map(concreteToAbstractActor andThen (_.removeEnv))

        (concreteSpawnedLifted.size, (abstractSpawnedLifted -- concreteSpawnedLifted).size)

    protected def compareBehs(anl: Analysis, concreteState: ConcreteState): ObsSpu =
        val abstractBehLifted = anl.getBehaviors
            .map { case (actor, behaviors) =>
                val newActor = anl.view(actor).removeContext.removeEnv
                val newBehaviors = behaviors.map(_.removeEnv)
                newActor -> newBehaviors
            }
            .toMap
            .withDefaultValue(Set())

        val concreteBehLifted = concreteState.behs
            .map { case (actor, behaviors) =>
                val newActor = concreteToAbstractActor(actor).removeEnv.removeContext
                val newBehaviors = behaviors.map(_.removeEnv).toSet
                newActor -> newBehaviors
            }
            .toMap
            .withDefaultValue(Set())

        val spurious = abstractBehLifted.map { case (actor, behaviors) => (behaviors -- concreteBehLifted(actor)).size }.sum
        val observed = concreteBehLifted.map(_._2.size).sum
        (observed, spurious)

    protected def onBenchmark(benchmark: String): PrecisionResult =
        println(s"Analyzing $benchmark")
        val source = Reader.loadFile(benchmark)
        val program = parseProgram(source)
        // Run the concrete analysis a few times
        val concreteState = runConcrete(concreteRuns, program, benchmark)
        // Run the abstract
        val anl = analysis(program)
        try anl.analyzeWithTimeout(timeout)
        catch
            case _: TimeoutException => return PrecisionResult.TimedOut
            case _                   => return PrecisionResult.Fail

        // Compare the abstract state with the concrete state
        PrecisionResult.Result(compareMailbox(anl, concreteState) + compareSpawned(anl, concreteState) + compareBehs(anl, concreteState))

    def run(outCsv: String): Unit =
        val outputTable = benchmarks.foldLeft(Table.empty[String])((table, benchmark) =>
            val result = onBenchmark(benchmark)
            result match
                case PrecisionResult.TimedOut =>
                    table
                        .add(benchmark, "observed", "-")
                        .add(benchmark, "# spurious", "-")
                case PrecisionResult.Fail =>
                    table
                        .add(benchmark, "observed", "ERROR")
                        .add(benchmark, "# spurious", "ERROR")

                case PrecisionResult.Result(ObsSpu(observed, spurious)) =>
                    table
                        .add(benchmark, "observed", observed.toString)
                        .add(benchmark, "# spurious", spurious.toString)
        )

        val output = outputTable.toCSVString(rowName = "benchmark")
        val outputFile = Writer.openTimeStamped(outCsv)
        Writer.write(outputFile, output)

object SimpleModActorPrecisionBenchmarks extends ModActorPrecisionBenchmarks:
    type Analysis = SimpleSchemeModActorAnalysis

    val benchmarks: Set[String] = SchemeBenchmarkPrograms.actors

    def analysis(program: SchemeExp) = SchemeAnalyses.modActorAnalysis(program)
    def main(args: Array[String]): Unit =
        run("benchOutput/precision/modactor-precision.csv")
