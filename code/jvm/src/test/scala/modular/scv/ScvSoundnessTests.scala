package maf.test.modular.scv

import maf.cli.modular.scv.*
import maf.core.Identity
import maf.test.*
import maf.language.ContractScheme.*
import maf.language.ContractScheme.interpreter.ConcreteValues.*
import maf.language.ContractScheme.interpreter.{ConcreteValues, RandomInputsFromFile}
import maf.language.scheme.*
import maf.language.scheme.lattices.*
import maf.modular.*
import maf.modular.scv.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.modular.worklist.*
import maf.util.*
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.cli.experiments.SchemeAnalyses
import maf.language.ContractScheme.interpreter.ContractSchemeInterpreter
import maf.language.ContractScheme.interpreter.ContractSchemeErrors.*
import java.util.concurrent.TimeoutException
import maf.language.scheme.interpreter.ProgramError
import maf.language.scheme.interpreter.ChildThreadDiedException
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.scheme.interpreter.*
import maf.core.Position.PTag
import maf.core.Position.SourcePathTag

/**
 * Soft contract verification is regarded as sound if it generates contract violations for contracts that are violated (unsatisfied) at runtime.
 *
 * This set of tests are manual tests which are specifically designed to detect potential soundness issues in the analysis
 */
trait ScvHasContractViolationsSoundnessTests extends ScvAnalysisTests:
    def onBenchmark(b: Benchmark): Unit =
      property(s"$b should contain contract violations") {
        runFromFile(b) { an =>
          // TODO: this is not very precise and does not check where the actual error should be
          assert(an.summary.blames.size > 0)
        }
      }

class ScvSmallSoundnessTests extends ContractSoundnessTestsBenchmarks with ScvHasContractViolationsSoundnessTests

/**
 * Soundness tests based on the ModF soundness tests. Runs both the concrete and the abstract interpreter. The following should be true for any value
 * v' from the abstract interpreter:
 *
 * v ⊑ v' where v is the (joined) result of a concrete execution on a particular location i nthe code.
 *
 * To test for the soundness of blame errors, the location of the blame error is registered. The abstract interpreter should always register a blame
 * error if the concrete interpreter has triggered a blame error (soundness). If the concrete execution of the program never results in a blame error,
 * but the abstract interpreter registers one, then the abstract interpreter will be less precise (precision).
 *
 * ContractScheme contains a special expression called "OPQ". The concrete interpreter expects a random generator that can provide a uniform
 * distribution of Scheme values for this expression to evaluate to.
 *
 * @see
 *   maf.test.lattice.ConcreteGenerators
 */
trait ScvSoundnessTests extends SchemeSoundnessTests:
    private var blameResults: Set[(Identity, Identity)] = Set()

    /**
     * A contract program concrete execution is kickstarted by random inputs.
     *
     * These random inputs may come from multiple sources but in this soundness test they are coming from input/generated/.
     *
     * This is the reason why the concrete execution does not need to be run multiple times: the random inputs provide to the program should already
     * generated sufficient lien coverage.
     */

    override def concreteRuns(b: Benchmark): Int = 1

    override def createInterpreter(
        addResult: (Identity, ConcreteValues.Value) => Unit,
        io: IO = new EmptyIO(),
        benchmark: String
      ): SchemeInterpreter =
      ContractSchemeInterpreter(cb = addResult, generator = Some(RandomInputsFromFile(RandomInputsFromFile.toInputPath(benchmark))))

    override def parseProgram(txt: String, benchmark: String): SchemeExp =
      ContractSchemeParser.parse(txt, SourcePathTag(benchmark))

    override def handleInterpreterError(addResult: (Identity, ConcreteValues.Value) => Unit): PartialFunction[Throwable, Any] = {
      case ContractSchemeBlame(lcontract, lserver, _) => addResult(lcontract, ConcreteValues.ContractValue(ContractValues.Blame(lcontract, lserver)))
      case e                                          => throw e
    }

    /** View the given analysis as an ScvModAnalysis */
    protected def view(anl: Analysis): ScvModAnalysis = anl match
        case a: ScvModAnalysis => a
        case _                 => throw new Exception("the current analsyis cannot be viewed as an SCV analysis")

    // This should already be done automatically since blame errors are also added to the store
    ///override def compareResults(analysis: Analysis, concreteResults: Map[Identity, Set[Value]], message: String): Unit =
    //    val scvAnalysis = view(analysis)
    //    val allBlames = scvAnalysis.summary.blames.values.flatMap(blames => blames.map(blame => (blame.blamedPosition, blame.blamingPosition))).toSet

    //    // compare the results of the blame set with the blame set of the analysis
    //    val subsumes = blameResults.subsetOf(allBlames)

    //    if !subsumes then
    //        val failureMessage = "unsound" // TODO: refine error message
    //        if message.isEmpty then fail(failureMessage)
    //        else fail(s"$message > $failureMessage")
    //    else
    //        // Use the regular comparison for the others
    //        super.compareResults(analysis, concreteResults, message)
    //
    override def compareResults(analysis: Analysis, concreteResults: Map[Identity, Set[Value]], message: String): Unit =
      super.compareResults(analysis, concreteResults, message)

    override def checkSubsumption(analysis: Analysis)(v: Value, abs: analysis.Value): Boolean =
        import ConcreteValues.ContractValue

        import maf.language.ContractScheme.ContractValues.{Value => CValue, *}
        val lat = analysis.lattice
        lat.isOpq(abs) || (v match
            case ContractValue(b: Blame) => lat.subsumes(abs, lat.blame(b))
            case ContractValue(g: Grd[Value]) =>
              lat.getGrds(abs).exists { grd =>
                g.domain.zip(grd.domain).forall(checkSubsumption(analysis)(_, _)) &&
                checkSubsumption(analysis)(g.rangeMaker, grd.rangeMaker) && g.domainIdns == grd.domainIdns && g.rangeMakerExpr == grd.rangeMakerExpr
              }
            case ContractValue(a: Arr[Value]) =>
              lat.getArrs(abs).exists { arr =>
                a.lcontract == arr.lcontract &&
                a.lserver == arr.lserver &&
                checkSubsumption(analysis)(ContractValue(a.contract), lat.grd(arr.contract))
              }
            case ContractValue(f: Flat[Value]) =>
              lat.getFlats(abs).exists { fl =>
                checkSubsumption(analysis)(f.contract, fl.contract) && /* f.fexp == fl.fexp && */ f.contractIdn == fl.contractIdn
              }

            case ContractValue(Struct(tag, fields)) =>
              lat.getStructs(abs).exists { s => s.tag == tag && fields.contents.zip(s.fields.contents).forall(checkSubsumption(analysis)(_, _)) }
            case ContractValue(StructSetterGetter(tag, idx, isSetter)) =>
              lat.getGetterSetter(abs).exists { s => s.tag == tag && s.idx == idx && s.isSetter == isSetter }
            case ContractValue(StructConstructor(tag, size)) =>
              lat.getStructConstructor(abs).exists { s => s.tag == tag && s.size == size }
            case ContractValue(StructPredicate(tag)) =>
              lat.getStructPredicates(abs).exists { s => s.tag == tag }
            case _ =>
              // if there is a single opaque value in the abstract value, it supercedes the other values
              super.checkSubsumption(analysis)(v, abs)
        )

    def analysis(program: SchemeExp): Analysis =
      SchemeAnalyses.scvModAnalysisWithRacketFeatures(program)

/** Automated soundness tests  on the set of benchmarks from the Nguyen paper */
class ScvNguyenSoundnessTests extends ScvSoundnessTests:
    def name: String = "scv-soundness-tests"
    override def benchmarks: Set[String] = SchemeBenchmarkPrograms.scvNguyenBenchmarks
