package maf.test.persistence

import maf.core.Address
import maf.core.Expression
import maf.language.scheme.SchemeExp
import maf.modular.ModAnalysis
import maf.modular.scheme.modf.SchemeModFComponent
import maf.save.LoadAddr
import maf.save.LoadNoContext
import maf.save.LoadSchemeAddr
import maf.save.LoadSchemeExpressions
import maf.save.LoadStandardSchemeComponentPosition
import maf.save.SaveAddr
import maf.save.SaveNoContext
import maf.save.SaveSchemeAddr
import maf.save.SaveStandardSchemeComponentPosition
import maf.save.save.SaveSchemeExpressions
import org.scalacheck.Gen
import maf.modular.scheme.VarAddr
import maf.modular.scheme.modf.NoContext
import maf.modular.ReturnAddr
import maf.modular.scheme.PrmAddr
import maf.modular.scheme.PtrAddr
import io.bullet.borer.Encoder
import io.bullet.borer.Decoder
import maf.save.SaveContext
import maf.save.LoadContext
import maf.modular.AddrDependency
import maf.save.SaveAddrDep
import maf.save.LoadAddrDependency
import maf.modular.Dependency

trait AddressGenerator extends Generator with ComponentGenerator:
    val addresses =
        Gen.oneOf(functionGen(generateVarAddr), functionGen(generatePrmAddr), functionGen(generateReturnAddr), functionGen(generatePtrAddr))

    def generateVarAddr(): VarAddr[Option[StringValue]] =
        return new VarAddr(identifiers.sample.get, Gen.option(stringValues).sample.get)

    def generateReturnAddr(): ReturnAddr[StringValue] =
        return new ReturnAddr(StringValue(str.sample.get), identities.sample.get)

    def generatePrmAddr(): PrmAddr =
        return new PrmAddr(str.sample.get)

    def generatePtrAddr(): PtrAddr[Option[StringValue]] =
        return new PtrAddr(PersistenceSpec.simpleSchemeExpression(str.sample.get), Gen.option(stringValues).sample.get)

trait DependencyGenerator extends Generator:
    val dependencies = stringAddr.map(addr => new AddrDependency(addr))

class PersistDependencySpec extends PersistenceSpec with DependencyGenerator:
    class DepenencyAnalysis
        extends TestAnalysis
        with SaveAddrDep[Expression]
        with LoadAddrDependency[Expression]
        with SaveStringAddr[Expression]
        with LoadStringAddr[Expression]
        with SaveStringComponent[Expression]
        with LoadStringComponent[Expression]

    testEncodingDecoding(
      "dependency",
      dependencies,
      () =>
          val anl = new DepenencyAnalysis
          import anl.given
          (summon[Encoder[Dependency]], summon[Decoder[Dependency]]),
      (original: Dependency, decoded: Dependency) => decoded should equal(original)
    )

class PersistAddressSpec extends PersistenceSpec with AddressGenerator:
    trait AddressAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SaveAddr[Expr] with LoadAddr[Expr]
    class SchemeAddressAnalysis
        extends TestSchemeAnalysis
        with AddressAnalysis[SchemeExp]
        with SaveSchemeAddr
        with LoadSchemeAddr
        with SaveStringContext[SchemeExp]
        with LoadStringContext[SchemeExp]
        with SaveStringComponent[SchemeExp]
        with LoadStringComponent[SchemeExp]
        with SaveStringExpression[SchemeExp]
        with LoadStringSchemeExpression

    class NoContextAnalysis extends TestAnalysis with SaveNoContext[Expression] with LoadNoContext[Expression]

    testEncodingDecoding(
      "address",
      addresses,
      () =>
          val anl = new SchemeAddressAnalysis
          import anl.given
          (summon[Encoder[Address]], summon[Decoder[Address]]),
      (original: Address, decoded: Address) =>
          decoded.idn should equal(original.idn)
          decoded should equal(original)
    )

    testEncodingDecoding(
      "no context",
      NoContext,
      () =>
          val anl = new NoContextAnalysis
          import anl.given
          (summon[Encoder[NoContext.type]], summon[Decoder[NoContext.type]]),
      (_: NoContext.type, decoded: NoContext.type) => decoded should equal(NoContext)
    )
