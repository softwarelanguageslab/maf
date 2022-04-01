package maf.cli.experiments.precision

import maf.language.scheme.interpreter.{EmptyIO, IO, SchemeInterpreter}
import maf.language.ContractScheme.interpreter.{ConcreteValues, ContractSchemeInterpreter}
import maf.cli.experiments.SchemeAnalyses
import maf.util.*
import maf.cli.experiments.aam.AAMAnalyses
import maf.lattice.*
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.benchmarks.Timeout
import scala.concurrent.duration.*
import maf.language.scheme.*
import maf.core.Identity
import maf.language.ContractScheme.interpreter.ContractSchemeErrors.ContractSchemeBlame
import maf.language.ContractScheme.*
import maf.language.ContractScheme.ContractValues.*

/**
 * This experiment compares the precision of
 *   - ScvModF: a modular analysis approach to soft contract verification.
 *   - AAMModF: an (classical) AAM approach optimized with functtion call boundaries and global stores
 */
object ScvPrecisionComparison
    extends AnalysisComparisonAlt[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:

    def analyses: List[(SchemeExp => Analysis, String)] = List(
      //(SchemeAnalyses.scvModAnalysisWithRacketFeatures, "scv-modf"),
      //(SchemeAnalyses.scvModAnalysisWithRacketFeaturesWithSharedPathStore, "scv-modf-sharedps")
      //(AAMAnalyses.scvAAMFnCallBoundaries, "scv-aam")
    )

    override def createInterpreter(addResult: (Identity, ConcreteValues.Value) => Unit, io: IO = new EmptyIO()): SchemeInterpreter =
        new ContractSchemeInterpreter(addResult)

    override def handleInterpreterError(addResult: (Identity, ConcreteValues.Value) => Unit): PartialFunction[Throwable, Any] = {
        case ContractSchemeBlame(lcontract, lserver, _) => addResult(lcontract, ConcreteValues.ContractValue(Blame(lcontract, lserver)))
        case e                                          => throw e // rethrow error if not a blame
    }

    override protected def convertConcreteValue(value: ConcreteValues.Value): BaseValue = value match
        case ConcreteValues.ContractValue(b: Blame)                        => baseLattice.blame(b)
        case ConcreteValues.ContractValue(g: Grd[ConcreteValues.Value])    => baseLattice.grd(g.map(convertConcreteValue(_)))
        case ConcreteValues.ContractValue(a: Arr[ConcreteValues.Value])    => baseLattice.arr(a.map(convertConcreteValue(_)))
        case ConcreteValues.ContractValue(f: Flat[ConcreteValues.Value])   => baseLattice.flat(f.map(convertConcreteValue(_)))
        case ConcreteValues.ContractValue(o: Opq)                          => baseLattice.opq(o)
        case ConcreteValues.ContractValue(s: Struct[ConcreteValues.Value]) => baseLattice.struct(s.map(convertConcreteValue(_)))
        case ConcreteValues.ContractValue(sc: StructConstructor)           => baseLattice.structConstructor(sc)
        case ConcreteValues.ContractValue(sgs: StructSetterGetter)         => baseLattice.structSetterGetter(sgs)
        case ConcreteValues.ContractValue(sp: StructPredicate)             => baseLattice.structPredicate(sp)
        case _                                                             => super.convertConcreteValue(value)

    override protected def convertV(analysis: Analysis)(value: analysis.modularLatticeWrapper.modularLattice.Value): baseDomain.Value = value match
        case analysis.modularLatticeWrapper.modularLattice.Blames(b)                => baseDomain.Blames(b)
        case analysis.modularLatticeWrapper.modularLattice.Grds(g)                  => baseDomain.Grds(g.map(_.map(convertValue(analysis))))
        case analysis.modularLatticeWrapper.modularLattice.Arrs(a)                  => baseDomain.Arrs(a.map(_.map(convertValue(analysis))))
        case analysis.modularLatticeWrapper.modularLattice.Flats(f)                 => baseDomain.Flats(f.map(_.map(convertValue(analysis))))
        case analysis.modularLatticeWrapper.modularLattice.Opqs(o)                  => baseDomain.Opqs(o)
        case analysis.modularLatticeWrapper.modularLattice.Structs(s)               => baseDomain.Structs(s.map(_.map(convertValue(analysis))))
        case analysis.modularLatticeWrapper.modularLattice.StructConstructors(c)    => baseDomain.StructConstructors(c)
        case analysis.modularLatticeWrapper.modularLattice.StructSetterGetters(sgs) => baseDomain.StructSetterGetters(sgs)
        case analysis.modularLatticeWrapper.modularLattice.StructPredicates(p)      => baseDomain.StructPredicates(p)
        case _                                                                      => super.convertV(analysis)(value)

    private val benchmarks: List[String] = SchemeBenchmarkPrograms.scvNguyenBenchmarks.toList

    override def runs = 1

    override def parseProgram(txt: String): SchemeExp =
        SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)

    override def additionalCounts(analysisName: String, path: Benchmark, r1: ResultMap, r2: ResultMap): Unit =
        // the concrete results are in r2, so if a blame is in r1, but not in r1 then it is a false positive
        val nFalsePositives = r1.values
            .flatMap(r1Values =>
                baseDomain.schemeLattice
                    .getBlames(r1Values)
                    .map(r1Blame => if r2.values.flatMap(r2Value => baseDomain.schemeLattice.getBlames(r2Value)).toSet.contains(r1Blame) then 0 else 1)
            )
            .sum

        results = results.add(path, s"$analysisName" + "_# false positives", Result.Success(nFalsePositives))

    override def compareOrdered(r1: ResultMap, r2: ResultMap, check: Boolean = true): Set[Identity] =
        // override check flag: checking is already done in soundness tests (except for unrelated, but in scv this in unavoidable because of the many synthesized calls)
        super.compareOrdered(r1, r2, check = false)

    def main(args: Array[String]) =
        benchmarks.foreach(runBenchmark)
        println(results.prettyString(format = _.toString))
        val writer = Writer.open("benchOutput/precision/scv-precision-benchmarks.csv")
        Writer.write(writer, results.toCSVString(format = _.toString, rowName = "benchmark"))
        Writer.close(writer)
