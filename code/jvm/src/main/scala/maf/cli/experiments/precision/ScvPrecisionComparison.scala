package maf.cli.experiments.precision

import maf.language.scheme.interpreter.{EmptyIO, IO, SchemeInterpreter}
import maf.language.ContractScheme.interpreter.{ConcreteValues, ContractSchemeInterpreter}
import maf.cli.experiments.SchemeAnalyses
import maf.cli.experiments.aam.AAMAnalyses
import maf.lattice.*
import maf.util.benchmarks.Timeout
import maf.language.scheme.*
import maf.core.Identity
import maf.language.ContractScheme.interpreter.ContractSchemeErrors.ContractSchemeBlame
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
      (SchemeAnalyses.scvModAnalysisWithRacketFeatures, "scv-modf"),
      (AAMAnalyses.scvAAMFnCallBoundaries, "scv-aam")
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
