package maf.cli.experiments.performance

import maf.cli.experiments.SchemeAnalyses
import maf.cli.experiments.performance.PerformanceEvaluationJMHPersistence.AnalyzedProgram
import maf.cli.experiments.performance.PerformanceEvaluationJMHPersistence.Program
import maf.cli.experiments.performance.PerformanceEvaluationJMHPersistence.ProgramPath
import maf.cli.experiments.performance.PerformanceEvaluationJMHPersistence.SavedProgram
import maf.cli.experiments.performance.PerformanceEvaluationJMHPersistence.testSizeFile
import maf.core.Expression
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeExp
import maf.modular.AnalysisEntry
import maf.modular.ModAnalysis
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.scheme.modf.SimpleSchemeModFAnalysis
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.save.Load
import maf.save.LoadActualComponents
import maf.save.LoadActualExpressions
import maf.save.LoadAddrDependency
import maf.save.LoadComponentIntID
import maf.save.LoadComponents
import maf.save.LoadDependency
import maf.save.LoadExpressionIntID
import maf.save.LoadFIFOWorklist
import maf.save.LoadGlobalStore
import maf.save.LoadInitialized
import maf.save.LoadModularSchemeDomain
import maf.save.LoadNoContext
import maf.save.LoadSchemeAddr
import maf.save.LoadSchemeConstantPropagationDomain
import maf.save.LoadSchemeExpressions
import maf.save.LoadStandardSchemeComponentPosition
import maf.save.LoadStandardSchemeComponents
import maf.save.LoadWorklist
import maf.save.Save
import maf.save.SaveActualComponents
import maf.save.SaveAddrDep
import maf.save.SaveComponentIntID
import maf.save.SaveComponents
import maf.save.SaveDependency
import maf.save.SaveGlobalStore
import maf.save.SaveInitialized
import maf.save.SaveModularSchemeDomain
import maf.save.SaveNoContext
import maf.save.SaveSchemeAddr
import maf.save.SaveSchemeConstantPropagationDomain
import maf.save.SaveSequentialWorklist
import maf.save.SaveStandardSchemeComponentPosition
import maf.save.SaveStandardSchemeComponents
import maf.save.SaveWorklist
import maf.save.save.SaveActualExpressions
import maf.save.save.SaveExpressionID
import maf.save.save.SaveExpressions
import maf.save.save.SaveRecursiveSchemeExpressionsIntID
import maf.save.save.SaveSchemeExpressions
import maf.save.save.SaveWorklistExpressionsID
import maf.util.Reader
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.BenchmarkParams
import org.openjdk.jmh.infra.BenchmarkParamsL2
import org.openjdk.jmh.infra.IterationParams
import org.openjdk.jmh.profile.ExternalProfiler
import org.openjdk.jmh.profile.InternalProfiler
import org.openjdk.jmh.profile.Profiler
import org.openjdk.jmh.results.AggregationPolicy
import org.openjdk.jmh.results.BenchmarkResult
import org.openjdk.jmh.results.BenchmarkResultMetaData
import org.openjdk.jmh.results.IterationResult
import org.openjdk.jmh.results.Result
import org.openjdk.jmh.results.ScalarResult
import org.openjdk.jmh.util.Multimap

import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.util.ArrayList
import java.util.Collection
import java.util.concurrent.TimeUnit
import maf.save.save.SaveExpressionIntID
import maf.save.SaveCbor
import maf.save.LoadCbor
import maf.save.save.SaveMainSchemeBody
import maf.save.LoadMainSchemeBody

object PersistenceEvaluation:
    trait SaveEvaluation[Expr <: Expression]
        extends Save[Expr]
        with SaveInitialized[Expr]
        with SaveComponents[Expr]
        with SaveWorklist[Expr]
        with SaveGlobalStore[Expr]
        with SaveDependency[Expr]
        with SaveAddrDep[Expr]

    trait SaveModF
        extends SaveEvaluation[SchemeExp]
        with SaveStandardSchemeComponents
        with SaveModularSchemeDomain
        with SaveSchemeConstantPropagationDomain
        with SaveSchemeAddr
        with SaveSchemeExpressions
        with SaveNoContext[SchemeExp]
        with SaveSequentialWorklist[SchemeExp]
        with SaveMainSchemeBody

    trait LoadEvaluation[Expr <: Expression]
        extends Load[Expr]
        with LoadInitialized[Expr]
        with LoadComponents[Expr]
        with LoadWorklist[Expr]
        with LoadGlobalStore[Expr]
        with LoadDependency[Expr]
        with LoadAddrDependency[Expr]

    trait LoadModF
        extends LoadEvaluation[SchemeExp]
        with LoadStandardSchemeComponents
        with LoadModularSchemeDomain
        with LoadSchemeConstantPropagationDomain
        with LoadSchemeAddr
        with LoadFIFOWorklist[SchemeExp]
        with LoadSchemeExpressions
        with LoadNoContext[SchemeExp]
        with LoadMainSchemeBody

    trait SimpleModF
        extends SimpleSchemeModFAnalysis
        with SchemeModFNoSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp]
        with SaveModF
        with LoadModF

    class SimpleModFActual(program: SchemeExp)
        extends SimpleSchemeModFAnalysis(program)
        with SaveActualExpressions[SchemeExp]
        with LoadActualExpressions[SchemeExp]
        with SaveActualComponents[SchemeExp]
        with LoadActualComponents[SchemeExp]
        with SimpleModF

    class SimpleModFPositionComponents(program: SchemeExp)
        extends SimpleSchemeModFAnalysis(program)
        with SaveActualExpressions[SchemeExp]
        with LoadActualExpressions[SchemeExp]
        with SaveStandardSchemeComponentPosition
        with LoadStandardSchemeComponentPosition
        with SimpleModF

    class SimpleModFIDComponents(program: SchemeExp)
        extends SimpleSchemeModFAnalysis(program)
        with SaveActualExpressions[SchemeExp]
        with LoadActualExpressions[SchemeExp]
        with SaveComponentIntID[SchemeExp]
        with LoadComponentIntID[SchemeExp]
        with SimpleModF

    class SimpleModFIDExpressions(program: SchemeExp)
        extends SimpleSchemeModFAnalysis(program)
        with SaveExpressionIntID[SchemeExp]
        with LoadExpressionIntID[SchemeExp]
        with SaveActualComponents[SchemeExp]
        with LoadActualComponents[SchemeExp]
        with SimpleModF

    class SimpleModFIDSchemeExpressions(program: SchemeExp, override val maxASTHeight: Int)
        extends SimpleSchemeModFAnalysis(program)
        with SaveRecursiveSchemeExpressionsIntID
        with LoadExpressionIntID[SchemeExp]
        with SaveActualComponents[SchemeExp]
        with LoadActualComponents[SchemeExp]
        with SimpleModF

    class SimpleModFIDs(program: SchemeExp, override val maxASTHeight: Int)
        extends SimpleSchemeModFAnalysis(program)
        with SaveRecursiveSchemeExpressionsIntID
        with LoadExpressionIntID[SchemeExp]
        with SaveComponentIntID[SchemeExp]
        with LoadComponentIntID[SchemeExp]
        with SimpleModF

    class SimpleModFActualCbor(program: SchemeExp) extends SimpleModFActual(program) with SaveCbor[SchemeExp] with LoadCbor[SchemeExp]
    class SimpleModFPositionComponentsCbor(program: SchemeExp)
        extends SimpleModFPositionComponents(program)
        with SaveCbor[SchemeExp]
        with LoadCbor[SchemeExp]
    class SimpleModFIDComponentsCbor(program: SchemeExp) extends SimpleModFIDComponents(program) with SaveCbor[SchemeExp] with LoadCbor[SchemeExp]
    class SimpleModFIDExpressionsCbor(program: SchemeExp) extends SimpleModFIDExpressions(program) with SaveCbor[SchemeExp] with LoadCbor[SchemeExp]
    class SimpleModFIDSchemeExpressionsCbor(program: SchemeExp, maxASTHeight: Int)
        extends SimpleModFIDSchemeExpressions(program, maxASTHeight)
        with SaveCbor[SchemeExp]
        with LoadCbor[SchemeExp]
    class SimpleModFIDsCbor(program: SchemeExp, maxASTHeight: Int)
        extends SimpleModFIDs(program, maxASTHeight)
        with SaveCbor[SchemeExp]
        with LoadCbor[SchemeExp]

    final val simpleModFActual = "simpleModFActual"
    final val simpleModFPositionComponents = "simpleModFPositionComponents"
    final val simpleModFIDComponents = "simpleModFIDComponents"
    final val simpleModFIDExpressions = "simpleModFIDExpressions"
    final val simpleModFIDSchemeExpressions = "simpleModFIDSchemeExpressions"
    final val simpleModFIDs = "simpleModFIDs"

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
class PerformanceEvaluationJMHPersistence:
    @Benchmark
    def analyze(program: Program): ModAnalysis[SchemeExp] =
        program.analysis.analyze()
        return program.analysis

    @Benchmark
    def save(program: AnalyzedProgram): Unit = program.analyzedProgram.save(program.saveFile.toString())

    @Benchmark
    def load(program: SavedProgram): ModAnalysis[SchemeExp] =
        program.loadAnalysis.load(program.saveFile.toString())
        return program.loadAnalysis

object PerformanceEvaluationJMHPersistence:
    // Use a static path to ensure that this file can be accessed throughout different JVM forks, this is necessary to allow it to be read by the profiler
    final val testSizeFile =
        Paths.get(System.getProperty("java.io.tmpdir").asInstanceOf[String] + "/testMAFPersistenceFileSize.json").asInstanceOf[Path]
    @State(Scope.Benchmark)
    class ProgramPath:
        @Param(
          Array(
            "test/R5RS/ad/bst.scm",
            "test/R5RS/ad/queue.scm",
            "test/R5RS/ad/linear.scm",
            "test/R5RS/various/infinite-2.scm",
            "test/R5RS/various/mceval.scm",
            "test/R5RS/icp/icp_3_leval.scm",
            "test/R5RS/gambit/sboyer.scm",
            "test/R5RS/gambit/scheme.scm",
          )
        )
        var program: String = _
        @Param(
          Array(
            PersistenceEvaluation.simpleModFActual,
            PersistenceEvaluation.simpleModFPositionComponents,
            PersistenceEvaluation.simpleModFIDComponents,
            PersistenceEvaluation.simpleModFIDExpressions,
            PersistenceEvaluation.simpleModFIDs,
            PersistenceEvaluation.simpleModFIDSchemeExpressions
          )
        )
        var runAnalysis: String = _
        @Param(Array("0", "1", "2", "3", "5"))
        var maxASTHeight: Int = _
        @Param(Array("true", "false"))
        var cbor: Boolean = _

        @Setup(Level.Trial)
        def skipMaxASTHeight: Unit =
            if maxASTHeight != 0 && !(runAnalysis == PersistenceEvaluation.simpleModFIDSchemeExpressions || runAnalysis == PersistenceEvaluation.simpleModFIDs) then
                System.exit(0)

        def getAnalysis(
            program: SchemeExp,
            analysis: String,
            maxADTHeight: Int = maxASTHeight
          ): ModAnalysis[SchemeExp] =
            runAnalysis match {
                case PersistenceEvaluation.simpleModFActual =>
                    if cbor then return new PersistenceEvaluation.SimpleModFActualCbor(program)
                    else return new PersistenceEvaluation.SimpleModFActual(program)
                case PersistenceEvaluation.simpleModFPositionComponents =>
                    return if cbor then new PersistenceEvaluation.SimpleModFPositionComponentsCbor(program)
                    else new PersistenceEvaluation.SimpleModFPositionComponents(program)
                case PersistenceEvaluation.simpleModFIDComponents =>
                    return if cbor then new PersistenceEvaluation.SimpleModFIDComponentsCbor(program)
                    else new PersistenceEvaluation.SimpleModFIDComponents(program)
                case PersistenceEvaluation.simpleModFIDExpressions =>
                    return if cbor then new PersistenceEvaluation.SimpleModFIDExpressionsCbor(program)
                    else new PersistenceEvaluation.SimpleModFIDExpressions(program)
                case PersistenceEvaluation.simpleModFIDSchemeExpressions =>
                    return if cbor then new PersistenceEvaluation.SimpleModFIDSchemeExpressionsCbor(program, maxADTHeight)
                    else new PersistenceEvaluation.SimpleModFIDSchemeExpressions(program, maxADTHeight)
                case PersistenceEvaluation.simpleModFIDs =>
                    return if cbor then new PersistenceEvaluation.SimpleModFIDsCbor(program, maxADTHeight)
                    else new PersistenceEvaluation.SimpleModFIDs(program, maxADTHeight)
            }

        protected def getExpression(file: String): SchemeExp = return CSchemeParser.parseProgram(Reader.loadFile(s"../../$file"))
        protected def newSaveFile: Path =
            val saveFile = Files.createTempFile("maf", ".json")
            if saveFile == null then return throw IOException("Could not create new temporary file.")
            else return saveFile

    @State(Scope.Benchmark)
    class Program extends ProgramPath:
        var analysis: ModAnalysis[SchemeExp] = _

        @Setup(Level.Invocation)
        def loadProgram: Unit =
            // Only run the analysis once for each program, since this does not change based on how it would be saved
            if runAnalysis != PersistenceEvaluation.simpleModFActual || cbor then System.exit(0)
            analysis = getAnalysis(getExpression(program), runAnalysis)

    @State(Scope.Benchmark)
    class AnalyzedProgram extends ProgramPath:
        var analyzedProgram: ModAnalysis[SchemeExp] = _
        var saveFile: Path = _

        @Setup(Level.Invocation)
        def createSaveFile: Unit = saveFile = newSaveFile

        @TearDown(Level.Invocation)
        def removeSaveFile: Unit = Files.move(saveFile, testSizeFile, StandardCopyOption.REPLACE_EXISTING)

        @Setup(Level.Trial)
        def analyzeProgram: Unit =
            analyzedProgram = getAnalysis(getExpression(program), runAnalysis)
            analyzedProgram.analyze()

    @State(Scope.Benchmark)
    class SavedProgram extends ProgramPath:
        var expression: SchemeExp = _
        var analyzedProgram: ModAnalysis[SchemeExp] = _
        var saveFile: Path = _
        var loadAnalysis: ModAnalysis[SchemeExp] = _

        @TearDown(Level.Trial)
        def removeSaveFile: Unit = Files.deleteIfExists(saveFile)

        @Setup(Level.Trial)
        def loadProgram: Unit =
            expression = getExpression(program)
            loadAnalysis = getAnalysis(expression, runAnalysis)
            analyzedProgram = getAnalysis(expression, runAnalysis)
            analyzedProgram.analyze()
            saveFile = newSaveFile
            analyzedProgram.save(saveFile.toString())

/**
 * This is a profile to be used by JMH, and can be used if you add `-prof maf.cli.experiments.performance.MaxMemoryProfiler` to the JMH command. This
 * will add a profiler that looks at the file at `PerformanceEvaluationJMHPersistence.testSizeFile` and will report the generated file size in the JMH
 * report. This is used in order to test how large a file is after a benchmark is run for the persistence benchmarks.
 */
class MaxMemoryProfiler extends ExternalProfiler:
    override def addJVMInvokeOptions(params: BenchmarkParams): Collection[String] = return new ArrayList()
    override def addJVMOptions(params: BenchmarkParams): Collection[String] = return new ArrayList()
    override def allowPrintErr(): Boolean = true
    override def allowPrintOut(): Boolean = true
    override def beforeTrial(params: BenchmarkParams): Unit =
        params.getForks()
        return
    override def getDescription(): String = return "Get the generated file size"
    override def afterTrial(br: BenchmarkResult, pid: Long, stdOut: File, stdErr: File): Collection[_ <: Result[_]] =
        val results: Collection[ScalarResult] = new ArrayList();
        if Files.exists(PerformanceEvaluationJMHPersistence.testSizeFile) then
            results.add(
              new ScalarResult("Saved file size",
                               Files.size(PerformanceEvaluationJMHPersistence.testSizeFile).toDouble / 1000,
                               "kB",
                               AggregationPolicy.MAX
              )
            );
        Files.deleteIfExists(PerformanceEvaluationJMHPersistence.testSizeFile)
        return results
        Files.deleteIfExists(PerformanceEvaluationJMHPersistence.testSizeFile)
        return results
