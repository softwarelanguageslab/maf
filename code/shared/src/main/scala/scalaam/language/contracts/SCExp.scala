package scalaam.language.contracts

import scalaam.core.{Expression, Identifier, Identity, Label}
import scalaam.language.sexp.Value
import scalaam.util.PrettyPrinter

case object FLAT_CONTRACT         extends Label
case object HIGHER_ORDER_CONTRACT extends Label
case object DEPENDENT_CONTRACT    extends Label
case object MONITOR               extends Label
case object NIL                   extends Label
case object VALUE                 extends Label
case object BLAME                 extends Label
case object IDENTIFIER            extends Label
case object LAMBDA                extends Label
case object FUNCTION_AP           extends Label
case object RAISE                 extends Label
case object CHECK                 extends Label
case object OPQ                   extends Label
case object LETREC                extends Label
case object BEGIN                 extends Label
case object IF                    extends Label
case object SET                   extends Label

/**
  * A language for defining software contracts
  */
trait ScExp extends Expression {
  implicit class FreeVariablesList(list: List[Expression]) {
    def fv: Set[String] = list.foldLeft(Set[String]())((a, b) => a ++ b.fv)
  }

  def or(exp: ScExp): ScExp =
    ScFunctionAp(ScIdentifier("or", Identity.none), List(exp, this), Identity.none)

  def and(exp: ScExp): ScExp =
    ScFunctionAp(ScIdentifier("and", Identity.none), List(exp, this), Identity.none)

  def not(): ScExp =
    ScFunctionAp(ScIdentifier("not", Identity.none), List(this), Identity.none)

  def app(exps: List[ScExp]): ScExp =
    ScFunctionAp(this, exps, Identity.none)

  def prettyPrint(printer: PrettyPrinter): Unit = printer.print(toString)
}
trait ScContract extends ScExp
case class ScFlatContract(expression: ScExp, idn: Identity) extends ScContract {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = expression.fv

  /** A label indicating the type of an expression. */
  override def label: Label = FLAT_CONTRACT

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(expression)

  override def toString: String = s"(flat $expression)"
}

case class ScHigherOrderContract(domain: ScExp, range: ScExp, idn: Identity) extends ScContract {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = domain.fv ++ range.fv

  /** A label indicating the type of an expression. */
  override def label: Label = HIGHER_ORDER_CONTRACT

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(domain, range)

  override def toString: String = s"(~> $domain $range)"
}

case class ScDependentContract(domain: ScExp, rangeMaker: ScExp, idn: Identity) extends ScContract {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = domain.fv ++ rangeMaker.fv

  /** A label indicating the type of an expression. */
  override def label: Label = DEPENDENT_CONTRACT

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(domain, rangeMaker)

  override def toString: String = s"(~>d $domain $rangeMaker)"
}

case class ScIdentifier(name: String, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = Set(name)

  /** A label indicating the type of an expression. */
  override def label: Label = IDENTIFIER

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List()

  override def toString: String = name
}

trait ScLiterals extends ScExp

case class ScLambda(variables: List[ScIdentifier], body: ScExp, idn: Identity)
    extends ScLiterals
    with ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] =
    body.fv -- variables.fv

  /** A label indicating the type of an expression. */
  override def label: Label = LAMBDA

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(body)

  override def toString: String = s"(lambda (${variables.map(_.toString).mkString(" ")}) $body)"

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print(s"(lambda ${variables.map(_.toString).mkString(" ")}")
    printer.newIndent()
    body.prettyPrint(printer)
    printer.print(")")
    printer.exitIndent()
  }
}

case class ScLetRec(name: ScIdentifier, binding: ScExp, body: ScExp, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = (body.fv ++ binding.fv) - name.name

  /** A label indicating the type of an expression. */
  override def label: Label = LETREC

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(body, binding)

  override def toString: String = s"(letrec ($name $binding) $body)"

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print(s"(letrec ($name $binding)")
    printer.newIndent()
    body.prettyPrint(printer)
    printer.print(")")
    printer.exitIndent()
  }
}

case class ScValue(value: Value, idn: Identity) extends ScLiterals {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = Set()

  /** A label indicating the type of an expression. */
  override def label: Label = VALUE

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List()

  override def toString: String = s"$value"
}

case class ScFunctionAp(operator: ScExp, operands: List[ScExp], idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = operator.fv ++ operands.fv

  /** A label indicating the type of an expression. */
  override def label: Label = FUNCTION_AP

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(operator) ++ operands

  override def toString: String = s"($operator ${operands.map(_.toString).mkString(" ")})"
}

case class ScBegin(expressions: List[ScExp], idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = expressions.fv

  /** A label indicating the type of an expression. */
  override def label: Label = BEGIN

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[ScExp] = expressions

  override def toString: String = s"(begin ${expressions.map(_.toString).mkString(" ")})"

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print("(begin")
    printer.newIndent()
    for (expression <- expressions) {
      expression.prettyPrint(printer)
      printer.newline()
    }
    printer.print(")")
    printer.exitIndent()
  }
}

case class ScSet(variable: ScIdentifier, value: ScExp, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = value.fv

  /** A label indicating the type of an expression. */
  override def label: Label = SET

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(value)

  override def toString: String = s"(set! $variable $value)"
}

case class ScMon(contract: ScExp, expression: ScExp, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = contract.fv ++ expression.fv

  /** A label indicating the type of an expression. */
  override def label: Label = MONITOR

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(contract, expression)

  override def toString: String = s"(mon $contract $expression)"
}

case class ScCheck(contract: ScExp, returnValue: ScExp, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = contract.fv ++ returnValue.fv

  /** A label indicating the type of an expression. */
  override def label: Label = CHECK

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(contract, returnValue)
}

case class ScIf(condition: ScExp, consequent: ScExp, alternative: ScExp, idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = condition.fv ++ consequent.fv ++ alternative.fv

  /** A label indicating the type of an expression. */
  override def label: Label = IF

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(condition, consequent, alternative)

  override def toString: String = s"(if $condition $consequent $alternative)"

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print("(if")
    condition.prettyPrint(printer)
    printer.newIndent()
    consequent.prettyPrint(printer)
    printer.newline()
    alternative.prettyPrint(printer)
  }
}

case class ScRaise(error: String, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = Set()

  /** A label indicating the type of an expression. */
  override def label: Label = RAISE

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List()

  override def toString: String = s"(raise $error)"
}

case class ScBlame(blame: Identity, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = Set()

  /** A label indicating the type of an expression. */
  override def label: Label = BLAME

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List()
}

case class ScNil(idn: Identity = Identity.none) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = Set()

  /** A label indicating the type of an expression. */
  override def label: Label = NIL

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List()

  override def toString: String = "nil"
}

case class ScOpaque(idn: Identity, refinement: Set[String]) extends ScExp {

  /** The set of free variables appearing in this expression. */
  def fv: Set[String] = Set()

  /** A label indicating the type of an expression. */
  def label: Label = OPQ

  /** Returns the list of subexpressions of the given expression. */
  def subexpressions: List[Expression] = List()

  override def toString: String = "OPQ"
}
