package maf.test.modular.scv

import maf.cli.modular.scv.*
import maf.core.Identity
import maf.test.*
import maf.language.ContractScheme.*
import maf.language.ContractScheme.interpreter.ConcreteValues.*
import maf.language.ContractScheme.interpreter.ConcreteValues
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

    /** A contract programs often contain OPQ values, we will run them a few times to get different initial values */
    override def concreteRuns(b: Benchmark): Int = 10

    override def evalConcrete(program: SchemeExp, benchmark: Benchmark): Map[Identity, Set[Value]] =
        var results: Map[Identity, Set[Value]] = Map().withDefaultValue(Set())
        blameResults = Set()
        val interpreter = ContractSchemeInterpreter()
        val timeout = concreteTimeout(benchmark)
        val runs = concreteRuns(benchmark)

        try
            for _ <- 1 to runs do
                try interpreter.run(program, timeout)
                catch
                    case ContractSchemeBlame(lcontract, lserver, _) => blameResults = blameResults + ((lcontract, lserver))
                    case e                                          => throw e

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

        results

    /** View the given analysis as an ScvModAnalysis */
    protected def view(anl: Analysis): ScvModAnalysis = anl match
        case a: ScvModAnalysis => a
        case _                 => throw new Exception("the current analsyis cannot be viewed as an SCV analysis")

    override def compareResults(analysis: Analysis, concreteResults: Map[Identity, Set[Value]], message: String): Unit =
        val scvAnalysis = view(analysis)
        val allBlames = scvAnalysis.summary.blames.values.flatMap(blames => blames.map(blame => (blame.blamedPosition, blame.blamingPosition))).toSet

        // compare the results of the blame set with the blame set of the analysis
        val subsumes = blameResults.subsetOf(allBlames)

        if !subsumes then
            val failureMessage = "unsound" // TODO: refine error message
            if message.isEmpty then fail(failureMessage)
            else fail(s"$message > $failureMessage")
        else
            // Use the regular comparison for the others
            super.compareResults(analysis, concreteResults, message)

    override def checkSubsumption(analysis: Analysis)(v: Value, abs: analysis.Value): Boolean =
        import ConcreteValues.ContractValue

        import maf.language.ContractScheme.ContractValues.{Value => CValue, *}
        val lat = analysis.lattice
        v match
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
                checkSubsumption(analysis)(f.contract, fl.contract) && f.fexp == fl.fexp && f.contractIdn == fl.contractIdn
              }
            case Opq()               => true
            case Struct(tag, fields) => ???
            //lat.getStructs(abs).exists { s => s.tag == tag && fields.zip(abs.fields).forall(checkSubsumption(analysis)(_, _))

            case _ => super.checkSubsumption(analysis)(v, abs)

    def analysis(program: SchemeExp): Analysis =
      SchemeAnalyses.scvModAnalysisWithRacketFeatures(program)
