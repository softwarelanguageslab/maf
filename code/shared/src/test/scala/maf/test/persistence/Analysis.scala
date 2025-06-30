package maf.test.persistence

import io.bullet.borer
import io.bullet.borer.Decoder
import io.bullet.borer.Encoder
import io.bullet.borer.Json
import io.bullet.borer.Writer
import maf.core.Address
import maf.core.Expression
import maf.core.Identifier
import maf.core.NoCodeIdentityDebug
import maf.core.Position
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.SchemeBegin
import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeLambdaExp
import maf.language.scheme.SchemeVar
import maf.modular.AnalysisEntry
import maf.modular.ModAnalysis
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.scheme.modf.SimpleSchemeModFAnalysis
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.save.AbstractDecoder
import maf.save.AbstractEncoder
import maf.save.ArrayDecoder
import maf.save.ArrayEncoder
import maf.save.ArrayKeyEncoder
import maf.save.Load
import maf.save.LoadActualComponents
import maf.save.LoadActualExpressions
import maf.save.LoadComponents
import maf.save.LoadExpressions
import maf.save.LoadSchemeExpressions
import maf.save.LoadStandardSchemeComponents
import maf.save.MapDecoder
import maf.save.MapEncoder
import maf.save.Save
import maf.save.SaveActualComponents
import maf.save.SaveComponents
import maf.save.SaveStandardSchemeComponents
import maf.save.save.SaveActualExpressions
import maf.save.save.SaveExpressions
import maf.save.save.SaveSchemeExpressions
import maf.util.Reader
import maf.util.benchmarks.Timeout.T
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import maf.save.SaveValue
import maf.save.LoadValue
import maf.core.Lattice
import maf.core.Identity
import maf.core.SimpleIdentity
import maf.save.SaveContext
import maf.save.LoadContext
import maf.save.SaveAddr
import maf.save.LoadAddr
import maf.modular.scheme.PrmAddr
import maf.core.Environment
import maf.language.scheme.SchemeLambdaExp
import maf.core.Address
import maf.core.Position
import maf.modular.scheme.PtrAddr
import maf.core.Identifier
import maf.modular.scheme.VarAddr
import maf.modular.scheme.modf.BaseEvalM
import maf.modular.scheme.modf.SchemeModFComponent
import maf.modular.scheme.modf.BaseSchemeModFSemanticsM
import maf.core.BasicEnvironment
import maf.language.scheme.SchemeLambda
import maf.save.SaveModF
import maf.save.LoadModF
import maf.save.SaveEnvironment
import maf.save.LoadEnvironment
import maf.modular.GlobalStore
import maf.save.SaveInitialized
import maf.save.SaveWorklist
import maf.save.SaveGlobalStore
import maf.save.SaveDependency
import maf.save.SaveAddrDep
import maf.save.SaveModularSchemeDomain
import maf.save.SaveSchemeConstantPropagationDomain
import maf.save.SaveSchemeAddr
import maf.save.SaveNoContext
import maf.save.SaveSequentialWorklist
import maf.save.LoadInitialized
import maf.save.LoadWorklist
import maf.save.LoadGlobalStore
import maf.save.LoadDependency
import maf.save.LoadAddrDependency
import maf.save.LoadModularSchemeDomain
import maf.save.LoadSchemeConstantPropagationDomain
import maf.save.LoadSchemeAddr
import maf.save.LoadFIFOWorklist
import maf.save.LoadNoContext
import maf.save.SaveStandardSchemeComponentPosition
import maf.save.LoadStandardSchemeComponentPosition
import maf.save.SaveComponentIntID
import maf.save.LoadComponentIntID
import maf.save.save.SaveRecursiveSchemeExpressionsIntID
import maf.save.LoadExpressionIntID
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFActual
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFPositionComponents
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFIDComponents
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFIDExpressions
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFIDs
import org.scalatest.compatible.Assertion
import maf.save.save.SaveExpressionIntID
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFIDSchemeExpressions
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFActualCbor
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFPositionComponentsCbor
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFIDComponentsCbor
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFIDExpressionsCbor
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFIDSchemeExpressionsCbor
import maf.test.persistence.PersistenceSpecAnalysises.SimpleModFIDsCbor
import maf.save.SaveCbor
import maf.save.LoadCbor
import maf.save.save.SaveMainSchemeBody
import maf.save.LoadMainSchemeBody

trait Generator:
    protected case class StringValue(str: String)
    protected val optionStr = Gen.option(Gen.alphaLowerStr)
    protected val str = Gen.alphaLowerStr
    val stringAddr: Gen[Address] = str.map(str => new PrmAddr(str))
    val stringValues = functionGen(stringValue)
    val stringContexts = functionGen(stringValue)
    val stringComponents = functionGen(stringValue)
    val stringEnvironments = stringAddr.map(addr => new BasicEnvironment[Address](Map((str.sample.get, addr))))

    def stringValue(): StringValue = return new StringValue(str.sample.get)

    def functionGen[T](f: () => T): Gen[T] = Gen.const("").map(_ => f())
    protected val maxDepth = 5
    protected var depth = 0
    def maxDepthFunctionGen[T](generate: () => T, retry: () => T): Gen[T] =
        Gen.const("")
            .map(_ =>
                if depth < maxDepth then
                    depth += 1
                    val res = generate()
                    depth -= 1
                    res
                else retry()
            )

object PersistenceSpec:
    def simpleIdentity(tag: String): Identity = new SimpleIdentity(Position.Position(-2, 0, new Position.SimplePTag(tag)))
    def simpleExpression(tag: String): Expression = Identifier("", new SimpleIdentity(Position.Position(-2, 0, new Position.SimplePTag(tag))))
    def simpleSchemeExpression(tag: String): SchemeExp = SchemeVar(
      Identifier("", simpleIdentity(tag))
    )
    def simpleSchemeLambdaExpression(tag: String): SchemeLambdaExp =
        new SchemeLambda(None, List(), List(simpleSchemeExpression(tag)), None, simpleIdentity(tag))

trait PersistenceSpec extends AnyPropSpec with ScalaCheckPropertyChecks with TableDrivenPropertyChecks with GivenWhenThen with Matchers with Generator:

    /**
     * Class used for getting access to the encoding/decoding givens.
     *
     * @note
     *   This analysis shouldn't be used to run actual analyses since the run method is a noop.
     */
    class TestAnalysis extends ModAnalysis[Expression](PersistenceSpec.simpleExpression(str.sample.get)) with Save[Expression] with Load[Expression]:
        override def finished: Boolean = return true
        override def intraAnalysis(component: Component): IntraAnalysis = ???
        override def addToWorkList(cmp: Component): Unit = ???
        override def initialComponent: Component = ???
        override def expr(cmp: Component): Expression = ???
        override protected def run(timeout: T): Unit = return

    /**
     * Class used for getting access to the encoding/decoding givens using scheme expressions.
     *
     * @note
     *   This analysis shouldn't be used to run actual analyses since the run method is a noop.
     */
    class TestSchemeAnalysis
        extends ModAnalysis[SchemeExp](PersistenceSpec.simpleSchemeExpression(str.sample.get))
        with Save[SchemeExp]
        with Load[SchemeExp]:
        override def finished: Boolean = return true
        override def intraAnalysis(component: Component): IntraAnalysis = ???
        override def addToWorkList(cmp: Component): Unit = ???
        override def initialComponent: Component = ???
        override def expr(cmp: Component): SchemeExp = ???
        override protected def run(timeout: T): Unit = return

    abstract class TestBaseSchemeModFSemanticsAnalysis extends TestSchemeAnalysis with BaseSchemeModFSemanticsM:
        override def expr(cmp: Component): SchemeExp = ???
        override def intraAnalysis(cmp: Component): SchemeModFSemanticsIntra = ???
        override def allocCtx(
            clo: (SchemeLambdaExp, Environment[Address]),
            args: List[Value],
            call: Position.Position,
            caller: Component
          ): ComponentContext =
            ???
        override def allocPtr(exp: SchemeExp, cmp: Component): PtrAddr[AllocationContext] = ???
        override def allocVar(id: Identifier, cmp: Component): VarAddr[AllocationContext] = ???
        override def baseEnv: Env = ???
        implicit override lazy val baseEvalM: BaseEvalM[M] = ???
        override def newComponent(call: SchemeModFComponent.Call[ComponentContext]): Component = ???
        override def view(cmp: Component): SchemeModFComponent = ???
        // Members declared in maf.modular.GlobalStore
        override def store: Map[Addr, Value] = ???
        override def store_=(store: Map[Addr, Value]): Unit = ???

    trait SaveStringContext[Expr <: Expression] extends SaveContext[Expr]:
        override type EncodeContext = StringValue
        override given contextEncoder: Encoder[EncodeContext] with
            override def write(writer: Writer, context: EncodeContext): Writer =
                writer.writeMapOpen(1)
                writer.write("==TESTING== NO CONTEXT")
                writer.write(context.str)
                writer.writeMapClose()

    trait LoadStringContext[Expr <: Expression] extends LoadContext[Expr]:
        override type DecodeContext = StringValue
        override given contextDecoder: Decoder[DecodeContext] with
            override def read(reader: borer.Reader): DecodeContext =
                reader.readMapOpen(1)
                if !reader.tryReadString("==TESTING== NO CONTEXT") then return reader.unexpectedDataItem("==TESTING== NO CONTEXT")
                val str = reader.readString()
                reader.readBreak()
                return StringValue(str)

    trait SaveStringValue[Expr <: Expression] extends SaveValue[Expr]:
        override type Value = StringValue
        implicit override lazy val lattice: Lattice[Value] = ???
        override given valueEncoder: Encoder[Value] with
            override def write(writer: Writer, value: Value): Writer =
                writer.writeMapOpen(1)
                writer.write("==TESTING== NO VALUE")
                writer.write(value.str)
                writer.writeMapClose()

    trait LoadStringValue[Expr <: Expression] extends LoadValue[Expr]:
        override type Value = StringValue
        implicit override lazy val lattice: Lattice[Value] = ???
        override given valueDecoder: Decoder[Value] with
            override def read(reader: borer.Reader): Value =
                reader.readMapOpen(1)
                if !reader.tryReadString("==TESTING== NO VALUE") then return reader.unexpectedDataItem("==TESTING== NO VALUE")
                val str = reader.readString()
                reader.readBreak()
                return StringValue(str)

    trait SaveStringComponent[Expr <: Expression] extends SaveComponents[Expr]:
        override type Component = StringValue
        override given componentEncoder: Encoder[Component] with
            override def write(writer: Writer, component: Component): Writer =
                writer.writeMapOpen(1)
                writer.write("==TESTING== NO COMPONENT")
                writer.write(component.str)
                writer.writeMapClose()

    trait LoadStringComponent[Expr <: Expression] extends LoadComponents[Expr]:
        override type Component = StringValue
        override given componentDecoder: Decoder[Component] with
            override def read(reader: borer.Reader): Component =
                reader.readMapOpen(1)
                if !reader.tryReadString("==TESTING== NO COMPONENT") then return reader.unexpectedDataItem("==TESTING== NO COMPONENT")
                val str = reader.readString()
                reader.readBreak()
                return StringValue(str)

    trait SaveStringExpression[Expr <: Expression] extends SaveExpressions[Expr]:
        override given expressionEncoder: Encoder[Expr] with
            override def write(writer: Writer, expression: Expr): Writer =
                writer.writeMapOpen(1)
                if expression.isInstanceOf[SchemeLambdaExp] then writer.write("==TESTING== NO LAMBDA EXPRESSION")
                else writer.write("==TESTING== NO EXPRESSION")
                writer.write(expression.idn.pos.tag.show)
                writer.writeMapClose()

    trait LoadStringExpression[Expr <: Expression] extends LoadExpressions[Expr]:
        def simpleExpression(name: String): Expr
        def simpleLambdaExpression(name: String): Expr
        override given expressionDecoder: Decoder[Expr] with
            override def read(reader: borer.Reader): Expr =
                reader.readMapOpen(1)
                val lambdaExp = reader.tryReadString("==TESTING== NO LAMBDA EXPRESSION")
                if !lambdaExp && !reader.tryReadString("==TESTING== NO EXPRESSION") then return reader.unexpectedDataItem("==TESTING== NO EXPRESSION")
                val str = reader.readString()
                reader.readBreak()
                return if lambdaExp then simpleLambdaExpression(str) else simpleExpression(str)

    trait SaveStringAddr[Expr <: Expression] extends SaveAddr[Expr]:
        override given addressEncoder: Encoder[Address] with
            override def write(writer: Writer, addr: Address): Writer =
                writer.writeMapOpen(1)
                writer.write("==TESTING== NO ADDRESS")
                writer.write(addr.asInstanceOf[PrmAddr].nam)
                writer.writeBreak()

    trait LoadStringAddr[Expr <: Expression] extends LoadAddr[Expr]:
        override def addressDecoders: Set[(String, Decoder[? <: Address])] =
            Set(("==TESTING== NO ADDRESS", summon[Decoder[PrmAddr]]))
        given Decoder[PrmAddr] with
            override def read(reader: borer.Reader): PrmAddr =
                val str = reader.readString()
                return new PrmAddr(str)

    trait LoadStringSchemeExpression extends LoadStringExpression[SchemeExp]:
        override def simpleExpression(tag: String): SchemeExp = PersistenceSpec.simpleSchemeExpression(tag)
        override def simpleLambdaExpression(tag: String): SchemeExp = PersistenceSpec.simpleSchemeLambdaExpression(tag)

    trait SaveStringEnvironment[Expr <: Expression] extends SaveEnvironment[Expr] with SaveStringAddr[Expr]
    trait LoadStringEnvironment[Expr <: Expression] extends LoadEnvironment[Expr] with LoadStringAddr[Expr]

    /**
     * Test whether an object can be encoded/decoded and whether or not the decoded value is equal to the original object.
     *
     * @param name
     *   The name of the object
     * @param object
     *   The object to test
     * @param codec
     *   The encoder and decoder for these objects
     */
    def testEncodingDecoding[T, ASSERTION](
        name: String,
        obj: T,
        codec: () => (Encoder[T], Decoder[T]),
        eq: (original: T, decoded: T) => ASSERTION
      ): Unit =
        property(s"A ${name} should be encoded") {
            val encoded = Json.encode(obj)(using codec()._1).toByteArray
            encoded.length should be > (0)
        }

        property(s"A ${name} should be decoded") {
            val encoded = Json.encode(obj)(using codec()._1).toByteArray
            val decoded = Json.decode(encoded).to[T](using codec()._2)
            decoded.valueTry.isSuccess should be(true)
        }

        property(s"A ${name} should remain the same when encoding and decoding") {
            val encoded = Json.encode(obj)(using codec()._1).toByteArray
            val decoded = Json.decode(encoded).to[T](using codec()._2)
            decoded.valueTry.isSuccess should be(true)
            eq(obj, decoded.value)
        }

    /**
     * Test whether a type can be encoded/decoded and whether or not the decoded value is equal to the original object.
     *
     * @param name
     *   The name of the object
     * @param objects
     *   A generator to create new objects
     * @param codec
     *   The encoder and decoder for these objects
     */
    def testEncodingDecoding[T, ASSERTION](
        name: String,
        objects: Gen[T],
        codec: () => (Encoder[T], Decoder[T]),
        eq: (original: T, decoded: T) => ASSERTION,
        print: Boolean = true
      ): Unit =
        property(s"A ${name} should be encoded") {
            forAll(objects) { (obj: T) =>
                if print then Given(name + ": " + obj.toString())
                val encoded = Json.encode(obj)(using codec()._1).toByteArray
                encoded.length should be > (0)
            }
        }

        property(s"A ${name} should be decoded") {
            forAll(objects) { (obj: T) =>
                if print then Given(name + ": " + obj.toString())
                val encoded = Json.encode(obj)(using codec()._1).toByteArray
                val decoded = Json.decode(encoded).to[T](using codec()._2)
                decoded.valueTry.isSuccess should be(true)
            }
        }

        property(s"A ${name} should remain the same when encoding and decoding") {
            forAll(objects) { (obj: T) =>
                if print then Given(name + ": " + obj.toString())
                val encoded = Json.encode(obj)(using codec()._1).toByteArray
                val decoded = Json.decode(encoded).to[T](using codec()._2)
                eq(obj, decoded.value)
            }
        }

class PersistAnalysisSpec extends PersistenceSpec:
    val programs = Gen.oneOf[Path](getFilesList("test/R5RS/ad").appendedAll(getFilesList("test/R5RS/various")))

    def getFilesList(folder: String): List[Path] =
        val programsStream = Files.list(Paths.get(folder))
        return if programsStream == null then List() else programsStream.iterator().nn.asScala.toList

    class ContextInsensitiveSchemeAnalysis(program: SchemeExp)
        extends SaveAnalysis(program)
        with SchemeModFNoSensitivity
        with SchemeConstantPropagationDomain
        with FIFOWorklistAlgorithm[SchemeExp]
        with SaveModF
        with LoadModF

    abstract class SaveAnalysis(program: SchemeExp)
        extends SimpleSchemeModFAnalysis(program)
        with AnalysisEntry[SchemeExp]
        with Save[SchemeExp]
        with Load[SchemeExp]

    private def save[Analysis <: AnalysisEntry[SchemeExp]](program: SchemeExp, analysis: Analysis): Path =
        import analysis.given
        analysis.analyze()
        val saveFile = Files.createTempFile("maf", ".json")
        require(saveFile != null)
        analysis.save(saveFile.toString())
        return saveFile.asInstanceOf[Path]

    private def load[Analysis <: AnalysisEntry[_]](analysis: Analysis, saveFile: Path): Analysis =
        import analysis.given
        analysis.load(saveFile.toString())
        return analysis

    private def testSchemePrograms[ASSERTION, Analysis <: ModAnalysis[SchemeExp]](
        anl: (program: SchemeExp) => Analysis,
        testCorrectness: (result: Analysis, loadedResult: Analysis) => ASSERTION,
        runTest: ((Path) => Unit) => Unit,
        limitHeight: Int
      ): Unit =
        runTest((path: Path) =>
            val program = CSchemeParser.parseProgram(Reader.loadFile(path.toString()))
            whenever(program.height < limitHeight) {
                Given(path.toString())
                val analysis = anl(program)
                val saveFile = save(program, analysis)

                val loadAnalysis = load(anl(program), saveFile)

                Files.deleteIfExists(saveFile)
                testCorrectness(analysis, loadAnalysis)
            }
        )

    private def testAnalysis[ASSERTION, Analysis <: GlobalStore[SchemeExp]](
        name: String,
        anl: (program: SchemeExp) => GlobalStore[SchemeExp] & BaseSchemeModFSemanticsM,
        runTest: ((Path) => Unit) => Unit = (test: (Path) => Unit) => forAll(programs) { (path: Path) => test(path) },
        limitHeight: Int = Int.MaxValue
      ) =
        property(s"A programs result should remain the same when encoded and decoded for a $name") {
            testSchemePrograms(
              anl,
              (result: GlobalStore[SchemeExp], loadedResult: GlobalStore[SchemeExp]) =>
                  loadedResult.result shouldBe defined
                  loadedResult.result.get should equal(result.result.get),
              runTest,
              limitHeight
            )
        }

        property(s"A programs components should remain the same when encoded and decoded for a $name") {
            testSchemePrograms(
              anl,
              (result: GlobalStore[SchemeExp], loadedResult: GlobalStore[SchemeExp]) =>
                  loadedResult.visited.size should equal(result.visited.size)
                  // This is done inside of a loop to improve the errors given when a test fails
                  for component <- result.visited do loadedResult.visited should contain(component),
              runTest,
              limitHeight
            )
        }

        property(s"A programs dependencies should remain the same when encoded and decoded for a $name") {
            testSchemePrograms(
              anl,
              (result: GlobalStore[SchemeExp], loadedResult: GlobalStore[SchemeExp]) =>
                  loadedResult.deps.size should equal(result.deps.size)
                  // This is done inside of a loop to improve the errors given when a test fails
                  for dependency <- result.deps.keysIterator do
                      loadedResult.deps.keySet should contain(dependency)
                      loadedResult.deps.get(dependency) should equal(result.deps.get(dependency))
              ,
              runTest,
              limitHeight
            )
        }

        property(s"A programs store should remain the same when encoded and decoded for a $name") {
            testSchemePrograms(
              anl,
              (result: GlobalStore[SchemeExp], loadedResult: GlobalStore[SchemeExp]) =>
                  loadedResult.store.size should equal(result.store.size)
                  // This is done inside of a loop to improve the errors given when a test fails
                  for addr <- result.store.keySet do
                      loadedResult.store.keySet should contain(addr)
                      loadedResult.store.get(addr) should equal(result.store.get(addr))
              ,
              runTest,
              limitHeight
            )
        }

    testAnalysis("context insensitive modf analysis saving the actual expressions and components",
                 (program: SchemeExp) => new SimpleModFActual(program),
                 limitHeight = 20
    )

    testAnalysis(
      "context insensitive modf analysis saving the components as their positions and actual expressions",
      (program: SchemeExp) => new SimpleModFPositionComponents(program),
      limitHeight = 20
    )

    testAnalysis(
      "context insensitive modf analysis saving the components as integer IDs and actual expressions",
      (program: SchemeExp) => new SimpleModFIDComponents(program),
      limitHeight = 20
    )

    testAnalysis(
      "context insensitive modf analysis saving the actual components and expressions as integer IDs",
      (program: SchemeExp) => new SimpleModFIDExpressions(program),
      limitHeight = 20
    )

    var maxADTHeight = 0
    testAnalysis(
      s"context insensitive modf analysis saving the expressions as integer IDs recursively and saving the actual components",
      (program: SchemeExp) => new SimpleModFIDSchemeExpressions(program, maxADTHeight),
      (test: (Path) => Unit) =>
          forAll(programs, Gen.chooseNum(0, 15))({ (path: Path, maxADTHeight: Int) =>
              this.maxADTHeight = maxADTHeight
              Given(s"max ADT height: $maxADTHeight")
              test(path)
          })
    )

    testAnalysis(
      s"context insensitive modf analysis saving the expressions recursively as integer IDs and components as integer IDs",
      (program: SchemeExp) => new SimpleModFIDs(program, maxADTHeight),
      (test: (Path) => Unit) =>
          forAll(programs, Gen.chooseNum(0, 15))({ (path: Path, maxADTHeight: Int) =>
              this.maxADTHeight = maxADTHeight
              Given(s"max ADT height: $maxADTHeight")
              test(path)
          })
    )

    testAnalysis(
      "context insensitive modf analysis saving the actual expressions and components in CBOR",
      (program: SchemeExp) => new SimpleModFActualCbor(program),
      limitHeight = 500
    )

    testAnalysis(
      "context insensitive modf analysis saving the components as their positions and actual expressions in CBOR",
      (program: SchemeExp) => new SimpleModFPositionComponentsCbor(program),
      limitHeight = 500
    )

    testAnalysis(
      "context insensitive modf analysis saving the components as integer IDs and actual expressions in CBOR",
      (program: SchemeExp) => new SimpleModFIDComponentsCbor(program),
      limitHeight = 500
    )

    testAnalysis(
      "context insensitive modf analysis saving the actual components and expressions as integer IDs in CBOR",
      (program: SchemeExp) => new SimpleModFIDExpressionsCbor(program),
      limitHeight = 500
    )

    testAnalysis(
      s"context insensitive modf analysis saving the expressions as integer IDs recursively and saving the actual components in CBOR",
      (program: SchemeExp) => new SimpleModFIDSchemeExpressionsCbor(program, maxADTHeight),
      (test: (Path) => Unit) =>
          forAll(programs, Gen.chooseNum(0, 100))({ (path: Path, maxADTHeight: Int) =>
              this.maxADTHeight = maxADTHeight
              Given(s"max ADT height: $maxADTHeight")
              test(path)
          })
    )

    testAnalysis(
      s"context insensitive modf analysis saving the expressions recursively as integer IDs and components as integer IDs in CBOR",
      (program: SchemeExp) => new SimpleModFIDsCbor(program, maxADTHeight),
      (test: (Path) => Unit) =>
          forAll(programs, Gen.chooseNum(0, 100))({ (path: Path, maxADTHeight: Int) =>
              this.maxADTHeight = maxADTHeight
              Given(s"max ADT height: $maxADTHeight")
              test(path)
          })
    )

object PersistenceSpecAnalysises:
    trait SaveSpec[Expr <: Expression]
        extends Save[Expr]
        with SaveInitialized[Expr]
        with SaveComponents[Expr]
        with SaveWorklist[Expr]
        with SaveGlobalStore[Expr]
        with SaveDependency[Expr]
        with SaveAddrDep[Expr]

    trait SaveModF
        extends SaveSpec[SchemeExp]
        with SaveStandardSchemeComponents
        with SaveModularSchemeDomain
        with SaveSchemeConstantPropagationDomain
        with SaveSchemeAddr
        with SaveSchemeExpressions
        with SaveNoContext[SchemeExp]
        with SaveSequentialWorklist[SchemeExp]
        with SaveMainSchemeBody

    trait LoadSpec[Expr <: Expression]
        extends Load[Expr]
        with LoadInitialized[Expr]
        with LoadComponents[Expr]
        with LoadWorklist[Expr]
        with LoadGlobalStore[Expr]
        with LoadDependency[Expr]
        with LoadAddrDependency[Expr]

    trait LoadModF
        extends LoadSpec[SchemeExp]
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
