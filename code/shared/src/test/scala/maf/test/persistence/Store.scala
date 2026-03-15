package maf.test.persistence

import maf.save.SaveValue
import maf.modular.ModAnalysis
import maf.save.LoadValue
import maf.save.LoadModularDomain
import maf.core.Expression
import maf.language.scheme.SchemeExp
import maf.save.SaveLattice
import maf.save.LoadLattice
import maf.lattice.HMapKey
import maf.language.scheme.lattices.SchemeLattice
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.modular.scheme.ModularSchemeLatticeWrapper
import maf.lattice.ConstantPropagation
import org.scalacheck.Gen
import io.bullet.borer.Encoder
import io.bullet.borer.Decoder
import maf.lattice.HMap
import maf.lattice.interfaces.IntLattice
import scala.collection.mutable.HashMap
import maf.core.BasicEnvironment
import maf.core.Address
import maf.save.SaveModularSchemeDomain
import maf.save.SaveSchemeConstantPropagationDomain
import maf.save.LoadModularSchemeDomain

trait ValueGenerator extends Generator:
    type SchemeLattice = ModularSchemeLattice[?, ?, ?, ?, ?, ?, ?]
    val modularLattice = SchemeConstantPropagationDomain.modularLattice

    val schemeLattices: Gen[(HMapKey, SchemeLattice#Value)] = functionGen(generateSchemeLattice)
    val schemeLatticeMaps = Gen.mapOfN(3, schemeLattices)

    def generateSchemeLattice(): (HMapKey, SchemeLattice#Value) =
        val lattice = Gen
            .oneOf(
              functionGen(generateSchemeIntLattice),
              functionGen(generateSchemeBoolLattice),
              functionGen(generateSchemeStringLattice),
              functionGen(generateSchemeCharLattice),
              functionGen(generateSchemePrimLattice),
              functionGen(generateSchemeKontLattice),
              functionGen(generateSchemeSymbolLattice),
              maxDepthFunctionGen(generateSchemeConsLattice, generateSchemeLattice),
              maxDepthFunctionGen(generateSchemeVectorLattice, generateSchemeLattice),
              maxDepthFunctionGen(generateSchemeInputPortLattice, generateSchemeLattice),
              functionGen(generateSchemeClosureLattice),
              functionGen(generateSchemeNilLattice),
              functionGen(generateSchemeVoidLattice),
              functionGen(generateSchemePointerLattice)
            )
            .sample
            .get
        return lattice

    val hMaps = schemeLatticeMaps.map((map) => new HMap(map))

    def generateConstantSchemeLattice[T](gen: Gen[T]): Gen[ConstantPropagation.L[T]] =
        return Gen.oneOf(ConstantPropagation.Top, ConstantPropagation.Bottom, ConstantPropagation.Constant(gen.sample.get))

    def generateSchemeIntLattice(): (HMapKey, SchemeLattice#Int) =
        return (modularLattice.IntT, new modularLattice.Int(generateConstantSchemeLattice(Gen.Choose.chooseBigInt.choose(0, 1000000)).sample.get))

    def generateSchemeBoolLattice(): (HMapKey, SchemeLattice#Bool) =
        return (modularLattice.BoolT, new modularLattice.Bool(generateConstantSchemeLattice(Gen.prob(0.5)).sample.get))

    def generateSchemeStringLattice(): (HMapKey, SchemeLattice#Str) =
        return (modularLattice.StrT, new modularLattice.Str(generateConstantSchemeLattice(str.sample.get).sample.get))

    def generateSchemeCharLattice(): (HMapKey, SchemeLattice#Char) =
        return (modularLattice.CharT, new modularLattice.Char(generateConstantSchemeLattice(Gen.alphaChar).sample.get))

    def generateSchemeInputPortLattice(): (HMapKey, SchemeLattice#InputPort) =
        return (modularLattice.InputPortT, new modularLattice.InputPort(hMaps.sample.get))

    def generateSchemeKontLattice(): (HMapKey, SchemeLattice#Kont) =
        return (modularLattice.KontT,
                new modularLattice.Kont(
                  Gen.listOfN(10, stringComponents).sample.get.asInstanceOf[List[SchemeLattice#Kont]].toSet
                )
        )

    def generateSchemePrimLattice(): (HMapKey, SchemeLattice#Prim) =
        return (modularLattice.PrimT, new modularLattice.Prim(Gen.listOfN(5, str).sample.get.toSet))

    def generateSchemeClosureLattice(): (HMapKey, SchemeLattice#Clo) =
        return (modularLattice.CloT,
                new modularLattice.Clo(
                  Set(
                    (PersistenceSpec.simpleSchemeLambdaExpression(str.sample.get),
                     new BasicEnvironment[Address](Map((str.sample.get, stringAddr.sample.get)))
                    )
                  )
                )
        )

    def generateSchemePointerLattice(): (HMapKey, SchemeLattice#Pointer) =
        return (modularLattice.PointerT, modularLattice.Pointer(Gen.listOfN(5, stringAddr).sample.get.toSet))

    def generateSchemeSymbolLattice(): (HMapKey, SchemeLattice#Symbol) =
        return (modularLattice.SymbolT, new modularLattice.Symbol(generateConstantSchemeLattice(str).sample.get))

    def generateSchemeConsLattice(): (HMapKey, SchemeLattice#Cons) =
        val car = hMaps.sample.get
        val cdr = hMaps.sample.get
        return (modularLattice.ConsT, new modularLattice.Cons(car, cdr))

    def generateSchemeVectorLattice(): (HMapKey, SchemeLattice#Vec) =
        val length = Gen.Choose.chooseBigInt.choose(1, 5).sample.get
        val lengthLattice = ConstantPropagation.Constant(length)

        val list = (for i <- Range(0, length.toInt) yield ((ConstantPropagation.Constant(BigInt(i)), hMaps.sample.get)))
        return (modularLattice.VecT, modularLattice.Vec(lengthLattice, list.toMap))

    def generateSchemeNilLattice(): (HMapKey, modularLattice.Nil.type) =
        return (modularLattice.NilT, modularLattice.Nil)

    def generateSchemeVoidLattice(): (HMapKey, modularLattice.Void.type) =
        return (modularLattice.VoidT, modularLattice.Void)

class PersistValueSpec extends PersistenceSpec with ValueGenerator:
    trait ValueAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SaveValue[Expr] with LoadValue[Expr]
    class ModularDomainAnalysis
        extends TestBaseSchemeModFSemanticsAnalysis
        with ValueAnalysis[SchemeExp]
        with SaveModularSchemeDomain
        with SaveSchemeConstantPropagationDomain
        with LoadModularDomain
        with LoadModularSchemeDomain
        with SaveStringContext[SchemeExp]
        with LoadStringContext[SchemeExp]
        with SaveStringComponent[SchemeExp]
        with LoadStringComponent[SchemeExp]
        with SaveStringExpression[SchemeExp]
        with LoadStringSchemeExpression
        with SaveStringAddr[SchemeExp]
        with LoadStringAddr[SchemeExp]:
        override val modularLatticeWrapper: ModularSchemeLatticeWrapper = SchemeConstantPropagationDomain

    testEncodingDecoding(
      "lattice",
      schemeLattices,
      () =>
          val anl = ModularDomainAnalysis()
          import anl.given
          (summon[Encoder[(HMapKey, Any)]], summon[Decoder[(HMapKey, Any)]]),
      (original: (HMapKey, Any), decoded: (HMapKey, Any)) =>
          decoded._1 should equal(original._1)
          decoded._2 should equal(original._2),
      false
    )
