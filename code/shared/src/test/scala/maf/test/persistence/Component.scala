package maf.test.persistence

import org.scalatest.flatspec.AnyFlatSpec
import maf.core.Position.Position
import io.bullet.borer.Json
import maf.save.SavePosition
import maf.language.scheme.SchemeExp
import maf.modular.ModAnalysis
import maf.save.AbstractEncoder
import maf.save.MapEncoder
import maf.core.Expression
import org.scalatest.BeforeAndAfterEach
import maf.core.Identifier
import maf.core.NoCodeIdentityDebug
import maf.save.ArrayEncoder
import maf.save.ArrayKeyEncoder
import maf.save.LoadPosition
import maf.save.AbstractDecoder
import maf.save.MapDecoder
import org.scalatest.Assertions._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import maf.core.Position.PTag
import maf.core.Position.NoPTag
import org.scalatest.GivenWhenThen
import maf.core.Position.SimplePTag
import maf.core.Position.PTagWithSource
import maf.core.Position.SourcePathTag
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import io.bullet.borer.Encoder
import maf.save.ArrayDecoder
import io.bullet.borer.Decoder
import maf.core.BasicEnvironment
import maf.core.NestedEnv
import maf.save.LoadContext
import maf.save.SaveContext
import maf.save.SaveNoContext
import maf.save.LoadNoContext
import maf.modular.scheme.modf.NoContext
import maf.core.NoCodeIdentity
import maf.modular.scheme.modf.SchemeModFComponent
import maf.core.Address
import maf.save.SaveEnvironment
import maf.save.LoadEnvironment
import maf.core.Environment
import maf.save.SaveStandardSchemeComponents
import maf.save.LoadStandardSchemeComponents
import maf.save.SaveActualComponents
import maf.save.LoadActualComponents
import maf.core.worklist.FIFOWorkList
import scala.collection.immutable.Queue
import maf.save.SaveWorklist
import maf.save.LoadFIFOWorklist
import maf.core.worklist.WorkList
import maf.util.benchmarks.Timeout.T
import maf.save.SaveSequentialWorklist

trait ComponentGenerator extends Generator:
    val pTags = Gen.alphaLowerStr.map((str) =>
        Gen.oneOf[PTag](NoPTag, new SimplePTag(str), new PTagWithSource(str, Gen.alphaLowerStr.sample.get), new SourcePathTag(str)).sample.get
    )

    val positions = (for
        x <- Gen.posNum[Int]
        y <- Gen.posNum[Int]
        ptag <- pTags
    yield Position(x, y, ptag))

    val identities = Gen.oneOf(NoCodeIdentity, NoCodeIdentityDebug)
    val identifiers = Gen.alphaLowerStr.map((str) => new Identifier(str, identities.sample.get))

    var schemeModFComponents = Gen.oneOf[SchemeModFComponent](
      SchemeModFComponent.Main,
      str.map(str =>
          new SchemeModFComponent.Call[StringValue]((PersistenceSpec.simpleSchemeLambdaExpression(str), stringEnvironments.sample.get),
                                                    stringContexts.sample.get
          )
      )
    )

trait EnvironmentGenerator extends Generator:
    val environments = Gen.oneOf(
      str.map(str => new BasicEnvironment[Address](Map((str, stringAddr.sample.get)))),
      str.map(str => new NestedEnv[Address, Address](Map((str, stringAddr.sample.get)), Gen.option(stringAddr.sample.get).sample.get))
    )

class PersistEnvironmentSpec extends PersistenceSpec with EnvironmentGenerator:
    class EnvironmentAnalysis
        extends TestAnalysis
        with SaveEnvironment[Expression]
        with LoadEnvironment[Expression]
        with SaveStringAddr[Expression]
        with LoadStringAddr[Expression]

    testEncodingDecoding(
      "environments",
      environments,
      () =>
          val analysis = new EnvironmentAnalysis
          import analysis.given
          (summon[Encoder[Environment[Address]]], summon[Decoder[Environment[Address]]]),
      (original: Environment[Address], decoded: Environment[Address]) => decoded should be(original)
    )

trait WorklistGenerator extends Generator:
    val FIFOWorklists = Gen.listOfN(10, stringComponents).map(comps => new FIFOWorkList[StringValue](Queue(), Set()).addAll(comps))

class PersistWorklistSpec extends PersistenceSpec with WorklistGenerator:
    class FIFOWorklistAnalysis
        extends TestAnalysis
        with SaveSequentialWorklist[Expression]
        with LoadFIFOWorklist[Expression]
        with SaveStringComponent[Expression]
        with LoadStringComponent[Expression]:
        override def addToWorkList(cmp: Component) = workList = workList.add(cmp)
        override def finished: Boolean = return true
        override def run(timeout: T): Unit = return
        override def initialComponent: Component = StringValue("INITIAL")

    testEncodingDecoding(
      "FIFO worklist",
      FIFOWorklists,
      () =>
          val analysis = new FIFOWorklistAnalysis
          import analysis.given
          (analysis.worklistEncoder, analysis.worklistDecoder),
      (original: WorkList[_], decoded: WorkList[_]) =>
          decoded.toList.size should equal(original.toList.size)
          var orig = original
          var dec = decoded
          while !orig.isEmpty do
              dec.isEmpty should be(false)
              dec.head should equal(orig.head)
              orig = orig.tail
              dec = dec.tail
          dec.isEmpty should be(true)
    )

class PersistComponentSpec extends PersistenceSpec with ComponentGenerator:
    class PositionAnalysis extends TestAnalysis with SavePosition[Expression] with LoadPosition[Expression]
    testEncodingDecoding(
      "position",
      positions,
      () =>
          val analysis = new PositionAnalysis
          import analysis.given
          (summon[Encoder[Position]], summon[Decoder[Position]]),
      (original: Position, decoded: Position) =>
          decoded.line should be(original.line)
          decoded.col should be(original.col)
          decoded.tag should be(original.tag)
    )

    class SchemeModFComponentAnalysis
        extends TestBaseSchemeModFSemanticsAnalysis
        with SaveStandardSchemeComponents
        with LoadStandardSchemeComponents
        with SaveActualComponents[SchemeExp]
        with LoadActualComponents[SchemeExp]
        with SaveStringEnvironment[SchemeExp]
        with LoadStringEnvironment[SchemeExp]
        with SaveStringContext[SchemeExp]
        with LoadStringContext[SchemeExp]
        with SaveStringExpression[SchemeExp]
        with LoadStringSchemeExpression:
        override lazy val initialComponent: Component = SchemeModFComponent.Main
        override def newComponent(call: SchemeModFComponent.Call[ComponentContext]): Component = ???
        override def view(cmp: Component): SchemeModFComponent = ???
        // Members declared in maf.modular.scheme.SchemeDomain
        implicit override lazy val lattice: maf.language.scheme.lattices.SchemeLattice[Value, maf.core.Address] = ???
        override lazy val primitives: maf.language.scheme.primitives.SchemePrimitives[Value, maf.core.Address] = ???

    testEncodingDecoding(
      "scheme modF components",
      schemeModFComponents,
      () =>
          val analysis = new SchemeModFComponentAnalysis
          import analysis.given
          (analysis.componentEncoder, analysis.componentDecoder),
      (original: SchemeModFComponent, decoded: SchemeModFComponent) => decoded should be(original)
    )

trait ContextGenerator:
    val contexts = Gen.const(NoContext)

class PersistContextSpec extends PersistenceSpec with ContextGenerator:
    class NoContextAnalysis extends TestAnalysis with SaveNoContext[Expression] with LoadNoContext[Expression]
    testEncodingDecoding(
      "no context",
      NoContext,
      () =>
          val analysis = new NoContextAnalysis
          import analysis.given
          (summon[Encoder[NoContext.type]], summon[Decoder[NoContext.type]]),
      (original: NoContext.type, decoded: NoContext.type) => original should equal(decoded)
    )
