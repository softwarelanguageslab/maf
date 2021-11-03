package maf.modular.incremental

import akka.io.Tcp.SO.KeepAlive
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.*
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.language.sexp.Value
import maf.modular.incremental.ProgramVersionExtracter.getVersion
import maf.util.{Reader, Writer}

import scala.util.Random

/** Automatically add change expressions to programs. */
// TODO: Avoid the same mutation to be generated multiple times? (i.e., filter duplicates)?
object ProgramChanger {

  private val rand = new Random()

  private enum ExpressionAction:
      case Add, Remove, Swap, NoChange

  private enum IfAction:
      case NegatePredicate, SwapBranches, Retain

  import ExpressionAction.*
  import IfAction.*

  // Keep numbers
  var removed: Int = 0
  var added: Int = 0
  var swaps: Int = 0
  var negatedPredicate: Int = 0
  var branchesSwapped: Int = 0

  // Gets an ExpressionAction with a certain probability:
  // None: 70%
  // Add: 7.5%
  // Remove: 10% but cannot always be executed.
  // Swap: 12.5% but cannot always be executed.
  private def getExpressionAction(): ExpressionAction =
      val n = rand.nextDouble()
      if n < 0.7 then NoChange
      else if n < 0.775 then Add
      else if n < 0.875 then Remove
      else Swap

  // Creates a display expression.
  private def createDisplayExp(exp: String): SchemeExp =
    SchemeFuncall(SchemeVar(Identifier("display", Identity.none)), List(SchemeVar(Identifier(exp, Identity.none))), Identity.none)
  private def createDisplayExp(exp: SchemeExp) = SchemeFuncall(SchemeVar(Identifier("display", Identity.none)), List(exp), Identity.none)

  // Gets an IfAction with a certain probability:
  // None: 90%
  // Negate predicate: 10%
  private def getIfAction(): IfAction =
      val n = rand.nextDouble()
      if n < 0.85 then Retain
      else if n < 0.925 then SwapBranches
      else NegatePredicate

  private def createNotExp(exp: SchemeExp) = SchemeFuncall(SchemeVar(Identifier("not", Identity.none)), List(exp), Identity.none)

  private def changeIf(ifE: SchemeIf, nested: Boolean): SchemeExp =
    getIfAction() match {
      case Retain => SchemeIf(changeExpression(ifE.cond, nested), changeExpression(ifE.cons, nested), changeExpression(ifE.alt, nested), ifE.idn)
      case NegatePredicate if nested =>
        negatedPredicate += 1
        SchemeIf(createNotExp(changeExpression(ifE.cond, nested)), changeExpression(ifE.cons, nested), changeExpression(ifE.alt, nested), ifE.idn)
      case NegatePredicate =>
        negatedPredicate += 1
        val pred = changeExpression(ifE.cond, true)
        SchemeIf(SchemeCodeChange(pred, createNotExp(pred), Identity.none),
                 changeExpression(ifE.cons, nested),
                 changeExpression(ifE.alt, nested),
                 ifE.idn
        )
      case SwapBranches if nested =>
        branchesSwapped += 1
        SchemeIf(changeExpression(ifE.cond, nested), changeExpression(ifE.alt, nested), changeExpression(ifE.cons, nested), ifE.idn)
      case SwapBranches =>
        branchesSwapped += 1
        SchemeIf(
          changeExpression(ifE.cond, nested),
          SchemeCodeChange(ifE.cons, changeExpression(ifE.alt, true), Identity.none),
          SchemeCodeChange(ifE.alt, changeExpression(ifE.cons, true), Identity.none),
          ifE.idn
        )
    }

  // Gets a random expression of the body to add, or returns a print expression.
  private def getExpressionToAdd(body: List[SchemeExp]): SchemeExp =
      val choices = body.flatMap(_.allSubexpressions) ++ body // Add body as these expressions are not part of the subexpressions.
      val choice = choices(rand.nextInt(choices.length))
      val result = if choice.isInstanceOf[SchemeExp] then choice.asInstanceOf[SchemeExp] else SchemeVar(choice.asInstanceOf[Identifier])
      val n = rand.nextDouble()
      if n < 0.25 then // Add a random print statement.
          createDisplayExp(result)
      else result

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
      case (l, Add) if nested => added += 1; changeExpression(getExpressionToAdd(fullbody), nested) :: changeBody(l, fullbody, nested)
      case (l, Add) =>
        added += 1
        SchemeCodeChange(SchemeValue(Value.Nil, Identity.none), changeExpression(getExpressionToAdd(fullbody), true), Identity.none) :: changeBody(
          l,
          fullbody,
          nested
        )

      // Swap expressions.
      case (l @ (h :: Nil), Swap)                => l // When there is only one statement, don't do anything (previously, it would equal add).
      case (l @ (h1 :: h2 :: t), Swap) if nested => swaps += 1; changeExpression(h2, nested) :: changeBody(h1 :: t, fullbody, nested)
      // TODO: let h1 swap with any of h2 :: t as in the case above.
      case (l @ (h1 :: h2 :: t), Swap) =>
        swaps += 1
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
    case ifE @ SchemeIf(cond, cons, alt, idn)              => changeIf(ifE, nested)
    case SchemeLet(bindings, body, idn) =>
      SchemeLet(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, body, nested), idn)
    case SchemeLetStar(bindings, body, idn) =>
      SchemeLetStar(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, body, nested), idn)
    case SchemeLetrec(bindings, body, idn) =>
      SchemeLetrec(bindings.map(bnd => (bnd._1, changeExpression(bnd._2, nested))), changeBody(body, body, nested), idn)
    case SchemeSet(variable, value, idn) => SchemeSet(variable, changeExpression(value, nested), idn)
    case SchemeBegin(exps, idn)          => SchemeBegin(changeBody(exps, exps, nested), idn)
    //case SchemeDefineVariable(name, value, idn) => SchemeDefineVariable(name, changeExpression(value, nested), idn)
    //case SchemeVar(id) =>
    //case SchemeValue(value, idn) =>
    case exp => exp

  }

  def changeBodyStatements(in: String, out: String, previouslyGenerated: List[String]): Option[String] =
      removed = 0
      added = 0
      swaps = 0
      negatedPredicate = 0
      branchesSwapped = 0
      val parsed = CSchemeParser.undefine(CSchemeParser.parse(Reader.loadFile(in)))
      val newProgram = changeExpression(parsed, false).prettyString()
      // Don't write the program if nothing has changed or the generated expression was a duplicate.
      if (removed + added + swaps + negatedPredicate != 0) && previouslyGenerated.find(_ == newProgram).isEmpty then
          val writer = Writer.open(out)
          Writer.writeln(
            writer,
            s"; Changes:\n; * removed: $removed\n; * added: $added\n; * swaps: $swaps\n; * negated predicates: $negatedPredicate\n; * swapped branches: $branchesSwapped"
          )
          Writer.write(writer, newProgram)
          Writer.close(writer)
          Some(newProgram) // Returns true if something has changed.
      else None
}

object Changer {

  def main(args: Array[String]): Unit =
      val inputFiles = SchemeBenchmarkPrograms.sequentialBenchmarks.toList // if args.isEmpty then List("test/R5RS/ad/quick.scm") else args.toList
      def outputFile(in: String, n: Int = 0) = "test/changes/scheme/generated/" ++ in.drop(5).dropRight(4).replace("/", "_").nn ++ s"-$n.scm"
      val amountToGenerate = 5
      println(s"Files to process: ${inputFiles.length}")
      for inputFile <- inputFiles do
          print(inputFile) // When an error is thrown, at least we will see which file caused problems.
          var times = amountToGenerate
          var programs: List[String] = Nil
          var tries = 0
          while times > 0 && tries < 5 * amountToGenerate do
              tries += 1
              ProgramChanger.changeBodyStatements(inputFile, outputFile(inputFile, times), programs).map { newProgram =>
                  times -= 1
                  programs = newProgram :: programs
              }
          println(s": generated $amountToGenerate programs using $tries attempts.")
      println("Finished.")
}
