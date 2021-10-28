package maf.modular.incremental

import akka.io.Tcp.SO.KeepAlive
import maf.core.*
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.language.sexp.Value
import maf.modular.incremental.ProgramVersionExtracter.getVersion
import maf.util.{Reader, Writer}

import scala.util.Random

/** Automatically add change expressions to programs. */
object ProgramChanger {

  private val rand = new Random()

  private enum ExpressionAction:
      case Add, Remove, Swap, NoChange

  import ExpressionAction.*

  // Gets an ExpressionAction with a certain probability:
  // None: 85%
  // Add: 5%
  // Remove: 5%
  // Swap: 5%
  private def getExpressionAction(): ExpressionAction =
      val n = rand.nextDouble()
      if n < 0.85 then NoChange
      else if n < 0.9 then Add
      else if n < 0.95 then Remove
      else Swap

  // Creates a display expression.
  private def createDisplayExp(exp: String): SchemeExp =
    SchemeFuncall(SchemeVar(Identifier("display", Identity.none)), List(SchemeVar(Identifier(exp, Identity.none))), Identity.none)
  private def createDisplayExp(exp: SchemeExp) = SchemeFuncall(SchemeVar(Identifier("display", Identity.none)), List(exp), Identity.none)

  // Gets a random expression of the body to add, or returns a print expression.
  private def getExpressionToAdd(body: List[SchemeExp]): SchemeExp =
      val n = rand.nextDouble()
      if n < 0.25 then // Add a random print statement.
          val fvs = body.flatMap(_.fv)
          createDisplayExp(fvs(rand.nextInt(fvs.length)))
      else body(rand.nextInt(body.length))

  // Swaps the first element of a list with a random element of the remainder of the list. A boolean indicates whether the last element should be eligible for swapping.
  // Note that when lastEligible is set to false, the first element will be selected again when the list only contains two elements.
  // Also returns the index of the swap, if there was a swap made.
  private def swapFirst[E](lst: List[E], lastEligible: Boolean): (List[E], Option[Int]) =
    if (!lastEligible && lst.length <= 2) || lst.length <= 1 then (lst, None)
    else
        val first = lst.head
        val otherIndex = rand.nextInt(if lastEligible then lst.length - 1 else lst.length - 2) + 1 // -1 & +1 ensure that another element is chosen (requires the list to have 2 elements at least).
        (lst.updated(0, lst(otherIndex)).updated(otherIndex, first), Some(otherIndex))

  // Keep numbers
  var removed: Int = 0
  var added: Int = 0
  var swaps: Int = 0

  private def changeBody(lst: List[SchemeExp], fullbody: List[SchemeExp], nested: Boolean): List[SchemeExp] =
    (lst, getExpressionAction()) match {
      case (Nil, _) => Nil

      // No changes.
      case (l @ (h :: Nil), NoChange) => changeExpression(h, nested) :: Nil
      case (h :: t, NoChange)         => changeExpression(h, nested) :: changeBody(t, fullbody, nested)

      // Remove an expression.
      case (h :: Nil, Remove)         => h :: Nil // Avoid empty bodies.
      case (h :: t, Remove) if nested => removed += 1; changeBody(t, fullbody, nested)
      case (h :: t, Remove) =>
        removed += 1; SchemeCodeChange(h, SchemeValue(Value.Nil, Identity.none), Identity.none) :: changeBody(t, fullbody, nested)

      // Add an expression.
      case (l @ (h :: t), Add) if nested =>
        added += 1; changeExpression(getExpressionToAdd(fullbody), nested) :: changeExpression(h, nested) :: changeBody(t, fullbody, nested)
      case (l @ (h :: t), Add) =>
        added += 1
        SchemeCodeChange(
          SchemeBegin(l, Identity.none),
          SchemeBegin(changeExpression(getExpressionToAdd(fullbody), true) :: l.map(changeExpression(_, true)), Identity.none),
          Identity.none
        ) :: changeBody(t, fullbody, nested)

      // Swap expressions.
      case (l @ (h :: Nil), Swap)        => l // When there is only one statement, don't do anything (previously, it would equal add).
      case (l @ (h1 :: h2 :: Nil), Swap) => changeExpression(h1, nested) :: changeExpression(h2, nested) :: Nil // Disallow swapping the last element.
      case (l @ (h1 :: h2 :: t), Swap) if nested => changeExpression(h2, nested) :: changeBody(h1 :: t, fullbody, nested)
      // TODO: let h1 swap with any of h2 :: t as in the case above.
      case (l @ (h1 :: h2 :: t), Swap) =>
        SchemeCodeChange(h1, changeExpression(h2, true), Identity.none) :: SchemeCodeChange(h2,
                                                                                            changeExpression(h1, true),
                                                                                            Identity.none
        ) :: changeBody(t, fullbody, nested)
    }

  // Nested indicated whether we are already in a changed expression (the "new" expression), and hence the changes can be made without introducing a change expression again.
  private def changeExpression(e: SchemeExp, nested: Boolean): SchemeExp = e match {
    case SchemeLambda(name, args, body, idn)               => SchemeLambda(name, args, changeBody(body, body, nested), idn)
    case SchemeVarArgLambda(name, args, vararg, body, idn) => SchemeVarArgLambda(name, args, vararg, changeBody(body, body, nested), idn)
    case SchemeFuncall(f, args, idn)                       => SchemeFuncall(f, args.map(changeExpression(_, nested)), idn)
    case SchemeIf(cond, cons, alt, idn) =>
      SchemeIf(changeExpression(cond, nested), changeExpression(cons, nested), changeExpression(alt, nested), idn)
    case SchemeLet(bindings, body, idn) =>
      SchemeLet(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, body, nested), idn)
    case SchemeLetStar(bindings, body, idn) =>
      SchemeLetStar(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, body, nested), idn)
    case SchemeLetrec(bindings, body, idn) =>
      SchemeLetrec(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, body, nested), idn)
    case SchemeSet(variable, value, idn)        => SchemeSet(variable, changeExpression(value, nested), idn)
    case SchemeBegin(exps, idn)                 => SchemeBegin(changeBody(exps, exps, nested), idn)
    case SchemeDefineVariable(name, value, idn) => SchemeDefineVariable(name, changeExpression(value, nested), idn)
    //case SchemeVar(id) =>
    //case SchemeValue(value, idn) =>
    case exp => exp

  }

  def changeBodyStatements(in: String, out: String): Unit =
      removed = 0
      added = 0
      swaps = 0
      val parsed = CSchemeParser.parseProgram(Reader.loadFile(in))
      val newProgram = changeExpression(parsed, false).prettyString()
      val writer = Writer.open(out)
      Writer.writeln(writer, s"; Changes:\n; * removed: $removed\n; * added: $added\n; * swaps: $swaps")
      Writer.write(writer, newProgram)
      Writer.close(writer)
}

object Changer {

  def main(args: Array[String]): Unit =
      val inputFile = "test/R5RS/ad/selsort.scm"
      def outputFile(n: Int = 0) = s"test/changes/scheme/generated/selsort-$n.scm"
      val times = 10
      for (i <- 0 to 10) do ProgramChanger.changeBodyStatements(inputFile, outputFile(i))
}
