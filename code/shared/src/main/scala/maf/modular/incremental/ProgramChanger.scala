package maf.modular.incremental

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

    private enum StatementAction:
        case Add, Remove, Swap, None

    import StatementAction.*

    // Gets a StatementAction with a certain probability:
    // None: 75%
    // Add: 5%
    // Remove: 10%
    // Swap: 10%
    private def getStatementAction(): StatementAction =
        val n = rand.nextDouble()
        if n < 0.75 then None
        else if n < 0.8 then Add
        else if n < 0.9 then Remove
        else Swap

    private def changeBody(lst: List[SchemeExp]): List[SchemeExp] =
       (lst, getStatementAction()) match {
            case (Nil, _) => Nil
            case (l@(h :: Nil), None) => l
            case (h :: t, None) => h :: changeBody(t)
            case (h :: t, Remove) => SchemeCodeChange(h, SchemeValue(Value.Nil, Identity.none), Identity.none) :: changeBody(t)
            case (l@(h :: t), Add) => SchemeCodeChange(SchemeBegin(l, Identity.none), SchemeBegin(h :: l, Identity.none), Identity.none) :: changeBody(t)
            case (l@(h :: Nil), Swap) => SchemeCodeChange(SchemeBegin(l, Identity.none), SchemeBegin(h :: l, Identity.none), Identity.none) :: Nil // When there is only one statement, swap equals add.
            case (l@(h1 :: h2 :: t), Swap) => SchemeCodeChange(h1, h2, Identity.none) :: SchemeCodeChange(h2, h1, Identity.none) :: changeBody(t)
        }

    private def changeStatements(e: SchemeExp): SchemeExp = e match {
        case SchemeLambda(name, args, body, idn)               => SchemeLambda(name, args, changeBody(body), idn)
        case SchemeVarArgLambda(name, args, vararg, body, idn) => SchemeVarArgLambda(name, args, vararg, changeBody(body), idn)
        case SchemeFuncall(f, args, idn)                       => SchemeFuncall(f, args.map(changeStatements), idn)
        case SchemeIf(cond, cons, alt, idn)                    => SchemeIf(changeStatements(cond), changeStatements(cons), changeStatements(alt), idn)
        case SchemeLet(bindings, body, idn)                    => SchemeLet(bindings.map(bnd => (bnd._1, changeStatements(bnd._2))), changeBody(body), idn)
        case SchemeLetStar(bindings, body, idn)          => SchemeLetStar(bindings.map(bnd => (bnd._1, changeStatements(bnd._2))), changeBody(body), idn)
        case SchemeLetrec(bindings, body, idn)           => SchemeLetrec(bindings.map(bnd => (bnd._1, changeStatements(bnd._2))), changeBody(body), idn)
        case SchemeSet(variable, value, idn)             => SchemeSet(variable, changeStatements(value), idn)
        case SchemeBegin(exps, idn)                      => SchemeBegin(changeBody(exps), idn)
        case SchemeDefineVariable(name, value, idn)      => SchemeDefineVariable(name, changeStatements(value), idn)

        case exp => exp

    }

    def changeBodyStatements(in: String, out: String): Unit =
        val parsed = CSchemeParser.parseProgram(Reader.loadFile(in))
        val newProgram = changeStatements(parsed).prettyString()
        val writer = Writer.open(out)
        Writer.write(writer, newProgram)
        Writer.close(writer)
}

object Changer {

    def main(args: Array[String]): Unit =
        val inputFile = "test/R5RS/ad/selsort.scm"
        def outputFile(n: Int = 0) = s"test/changes/scheme/generated/selsort-$n.scm"
        val times = 10
        for (i <- 0 to 10) do
            ProgramChanger.changeBodyStatements(inputFile, outputFile(i))
}
