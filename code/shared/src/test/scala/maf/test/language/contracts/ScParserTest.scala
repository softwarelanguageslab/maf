package maf.test.language.contracts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import maf.language.contracts.{
  SCExpCompiler,
  ScAssume,
  ScBegin,
  ScCheck,
  ScDependentContract,
  ScExp,
  ScFunctionAp,
  ScHigherOrderContract,
  ScIdentifier,
  ScIf,
  ScLambda,
  ScMon,
  ScOpaque,
  ScSet,
  ScValue
}
import maf.language.sexp.{SExpParser, ValueBoolean, ValueInteger}

import scala.collection.Set

class ScParserTest extends AnyFlatSpec with should.Matchers {
  private def compile(exp: String): ScExp = {
    val sexp = SExpParser.parse(exp)
    SCExpCompiler.compile(sexp.head)
  }

  "A number" should "parse to an ScValue" in {
    compile("1") should matchPattern { case ScValue(ValueInteger(1), _) => }
  }

  "A boolean" should "parse to an ScValue" in {
    compile("#t") should matchPattern { case ScValue(ValueBoolean(true), _)  => }
    compile("#f") should matchPattern { case ScValue(ValueBoolean(false), _) => }
  }

  "An identifier" should "parse to a ScIdentifier" in {
    compile("x") should matchPattern { case ScIdentifier("x", _) => }
  }

  "A lambda" should "parse to an ScLambda" in {
    compile("(lambda (x) x)") should matchPattern { case ScLambda(_, _, _) => }
  }

  "A lambda" should "have the correct arguments" in {
    val result = compile("(lambda (x) x)")
    result should matchPattern { case ScLambda(List(ScIdentifier("x", _)), _, _) => }
  }

  "A lambda" should "have the correct body" in {
    val result = compile("(lambda (x) x)")
    result should matchPattern { case ScLambda(_, ScIdentifier("x", _), _) => }
  }

  "An if" should "parse to an ScIf" in {
    compile("(if x y z)") should matchPattern {
      case ScIf(ScIdentifier("x", _), ScIdentifier("y", _), ScIdentifier("z", _), _) =>
    }
  }

  "A begin" should "parse to an ScBegin" in {
    compile("(begin x y z)") should matchPattern {
      case ScBegin(List(ScIdentifier("x", _), ScIdentifier("y", _), ScIdentifier("z", _)), _) =>
    }
  }

  "A mon" should "parse to an ScMon" in {
    compile("(mon x y)") should matchPattern {
      case ScMon(ScIdentifier("x", _), ScIdentifier("y", _), _) =>
    }
  }

  "A higher order contract" should "parse to a ScHigherOrderContract" in {
    compile("(~> x y)") should matchPattern {
      case ScHigherOrderContract(ScIdentifier("x", _), ScIdentifier("y", _), _) =>
    }
  }

  "A dependent contract" should "parse to a ScDependentContract" in {
    compile("(~ x y)") should matchPattern {
      case ScDependentContract(ScIdentifier("x", _), ScIdentifier("y", _), _) =>
    }
  }

  "A set!" should "parse to a ScSet" in {
    compile("(set! x 10)") should matchPattern {
      case ScSet(ScIdentifier("x", _), ScValue(ValueInteger(10), _), _) =>
    }
  }

  "A check" should "parse to a ScCheck" in {
    compile("(check int? 5)") should matchPattern {
      case ScCheck(ScIdentifier("int?", _), _, _) =>
    }
  }

  "An opaque" should "be able to have a refinement set" in {
    compile("(OPQ int?)") should matchPattern {
      case ScOpaque(_, refinements) if refinements.contains("int?") =>
    }
  }

  "An opaque" should "be able to be defined without refinements" in {
    compile("OPQ") should matchPattern {
      case ScOpaque(_, refinements) if refinements.isEmpty =>
    }
  }

  "An assumption" should "be able to be parsed" in {
    compile("(assume (x int?) (+ 1 1))") should matchPattern {
      case ScAssume(
          ScIdentifier("x", _),
          ScIdentifier("int?", _),
          ScFunctionAp(ScIdentifier("+", _), _, _),
          _
          ) =>
    }
  }

}
