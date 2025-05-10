package maf.test.persistence

import maf.save.save.SaveSchemeExpressions
import maf.save.LoadSchemeExpressions
import maf.save.save.SaveActualExpressions
import maf.save.LoadActualExpressions
import maf.language.scheme.SchemeExp
import org.scalacheck.Gen
import maf.language.scheme.SchemeVar
import maf.core.Identifier
import maf.core.NoCodeIdentityDebug
import io.bullet.borer.Encoder
import io.bullet.borer.Decoder
import maf.language.scheme.SchemeLambda
import maf.language.scheme.SchemeFuncall
import maf.language.scheme.SchemeVarArgLambda
import maf.language.scheme.SchemeLetrec
import maf.language.scheme.SchemeAssert
import maf.language.scheme.SchemeLet
import maf.language.scheme.SchemeIf
import maf.language.scheme.SchemeSet
import maf.language.scheme.SchemeBegin
import maf.language.scheme.SchemeLetStar
import maf.modular.ModAnalysis
import maf.core.Expression
import maf.language.scheme.SchemeValue
import maf.language.sexp.Value

trait ExpressionGenerator extends ComponentGenerator with Generator:
    val schemeExpressions: Gen[SchemeExp] = functionGen(generateSchemeExp)
    private val schemeExpressionList = Gen.listOfN(5, schemeExpressions)
    private val identifiersList = Gen.listOfN(10, identifiers)
    private val optionStrTuple = Gen.alphaLowerStr.map(str => Gen.option(Gen.const((str, Gen.alphaLowerStr.sample.get))).sample.get)
    private val bindings = Gen
        .listOfN(10, str)
        .map(names =>
            // This list is converted into a set to ensure that there are no duplicate identifiers
            Set(names: _*).toList.map(name => (new Identifier(name, identities.sample.get), generateSchemeExp()))
        )

    def maxDepthFunctionSchemeGen(generate: () => SchemeExp): Gen[SchemeExp] = maxDepthFunctionGen[SchemeExp](generate, generateSchemeExp)
    def generateSchemeExp(): SchemeExp =
        val res = Gen
            .frequency(
              (1, maxDepthFunctionSchemeGen(generateSchemeFuncall)),
              (10, identifiers.map(id => new SchemeVar(id))),
              (1, maxDepthFunctionSchemeGen(generateSchemeLambda)),
              (1, maxDepthFunctionSchemeGen(generateSchemeVarArgLambda)),
              (10, functionGen(generateSchemeValue)),
              (1, maxDepthFunctionSchemeGen(generateSchemeLetRec)),
              (1, maxDepthFunctionSchemeGen(generateSchemeAssert)),
              (1, maxDepthFunctionSchemeGen(generateSchemeLet)),
              (1, maxDepthFunctionSchemeGen(generateSchemeIf)),
              (1, maxDepthFunctionSchemeGen(generateSchemeSet)),
              (1, maxDepthFunctionSchemeGen(generateSchemeBegin)),
            )
            .sample
            .get
        return res

    def generateSchemeValue(): SchemeValue =
        return new SchemeValue(Value.String(str.sample.get), identities.sample.get)

    def generateSchemeLetStart(): SchemeLetStar =
        return new SchemeLetStar(bindings.sample.get, schemeExpressionList.sample.get, identities.sample.get)

    def generateSchemeBegin(): SchemeBegin =
        return new SchemeBegin(schemeExpressionList.sample.get, identities.sample.get)

    def generateSchemeSet(): SchemeSet =
        return new SchemeSet(identifiers.sample.get, schemeExpressions.sample.get, identities.sample.get)

    def generateSchemeIf(): SchemeIf =
        return new SchemeIf(schemeExpressions.sample.get, schemeExpressions.sample.get, schemeExpressions.sample.get, identities.sample.get)

    def generateSchemeLet(): SchemeLet =
        return new SchemeLet(bindings.sample.get, schemeExpressionList.sample.get, identities.sample.get)

    def generateSchemeAssert(): SchemeAssert =
        return new SchemeAssert(schemeExpressions.sample.get, identities.sample.get)

    def generateSchemeLetRec(): SchemeLetrec =
        return new SchemeLetrec(bindings.sample.get, schemeExpressionList.sample.get, identities.sample.get)

    def generateSchemeVarArgLambda(): SchemeVarArgLambda =
        return new SchemeVarArgLambda(optionStr.sample.get,
                                      identifiersList.sample.get,
                                      identifiers.sample.get,
                                      schemeExpressionList.sample.get,
                                      optionStrTuple.sample.get,
                                      identities.sample.get
        )

    def generateSchemeFuncall(): SchemeFuncall =
        return new SchemeFuncall(generateSchemeExp(), schemeExpressionList.sample.get, identities.sample.get)

    def generateSchemeLambda(): SchemeLambda =
        val str = Gen.alphaLowerStr.sample.get
        val id = identities.sample.get
        val lambda = new SchemeLambda(optionStr.sample.get, identifiersList.sample.get, schemeExpressionList.sample.get, optionStrTuple.sample.get, id)
        return lambda

class PersistExpressionSpec extends PersistenceSpec with ExpressionGenerator:
    trait ExpressionAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SaveActualExpressions[Expr] with LoadActualExpressions[Expr]
    class SchemeExpressionAnalysis extends TestSchemeAnalysis with ExpressionAnalysis[SchemeExp] with SaveSchemeExpressions with LoadSchemeExpressions

    testEncodingDecoding(
      "scheme expressions",
      schemeExpressions,
      () =>
          val anl = new SchemeExpressionAnalysis
          import anl.given
          (summon[Encoder[SchemeExp]], summon[Decoder[SchemeExp]]),
      (original: SchemeExp, decoded: SchemeExp) =>
          decoded.idn should equal(original.idn)
          decoded.height should equal(original.height)
          decoded should equal(original),
      false
    )
