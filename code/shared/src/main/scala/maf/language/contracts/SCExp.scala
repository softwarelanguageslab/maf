package maf.language.contracts

import maf.core.{Expression, Identifier, Identity, Label}
import maf.language.sexp.Value
import maf.util.{Monoid, PrettyPrinter}
import maf.language.scheme.SchemeExp

case object FLAT_CONTRACT extends Label
case object HIGHER_ORDER_CONTRACT extends Label
case object DEPENDENT_CONTRACT extends Label
case object MONITOR extends Label
case object NIL extends Label
case object VALUE extends Label
case object BLAME extends Label
case object IDENTIFIER extends Label
case object LAMBDA extends Label
case object FUNCTION_AP extends Label
case object RAISE extends Label
case object CHECK extends Label
case object OPQ extends Label
case object LETREC extends Label
case object BEGIN extends Label
case object IF extends Label
case object SET extends Label
case object ASSUME extends Label
case object PROGRAM extends Label
case object DEFINE extends Label
case object DEFINE_FN extends Label
case object DEFINE_ANNOTATED_FN extends Label
case object PROVIDE_CONTRACT extends Label
case object CONS extends Label
case object CAR extends Label
case object CDR extends Label
case object VARARG_IDENTIFIER extends Label

/** A language for defining software contracts */
trait ScExp extends SchemeExp {
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

  def prettyPrint(printer: PrettyPrinter): Unit = printer.print(toString, idn)
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

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print("(flat ", idn)
    expression.prettyPrint(printer)
    printer.print(")")
  }
}

case class ScHigherOrderContract(
    domain: ScExp,
    range: ScExp,
    idn: Identity)
    extends ScContract {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = domain.fv ++ range.fv

  /** A label indicating the type of an expression. */
  override def label: Label = HIGHER_ORDER_CONTRACT

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(domain, range)

  override def toString: String = s"(~> $domain $range)"
}

case class ScDependentContract(
    domains: List[ScExp],
    rangeMaker: ScExp,
    idn: Identity)
    extends ScContract {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = domains.flatMap(_.fv).toSet ++ rangeMaker.fv

  /** A label indicating the type of an expression. */
  override def label: Label = DEPENDENT_CONTRACT

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = rangeMaker :: domains

  override def toString: String = s"(~>d ${domains.map(_.toString)} $rangeMaker)"
}

trait ScParam extends ScExp {
  def name: String
}

case class ScIdentifier(name: String, idn: Identity) extends ScExp with ScParam {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = Set(name)

  /** A label indicating the type of an expression. */
  override def label: Label = IDENTIFIER

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List()

  override def toString: String = name
  override def prettyPrint(printer: PrettyPrinter): Unit =
    printer.print(toString, idn)
}

case class ScVarArgIdentifier(name: String, idn: Identity) extends ScExp with ScParam {
  override def label: Label = VARARG_IDENTIFIER

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = Set(name)

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List()
}

trait ScLiterals extends ScExp

case class ScLambda(
    variables: List[ScParam],
    body: ScExp,
    idn: Identity)
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
    printer.print(s"(lambda (${variables.map(_.toString).mkString(" ")})", idn)
    printer.newIndent()
    body.prettyPrint(printer)
    printer.print(")")
    printer.exitIndent()
  }
}

case class ScLetRec(
    names: List[ScIdentifier],
    bindings: List[ScExp],
    body: ScExp,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] =
    (body.fv ++ bindings.flatMap(_.fv).toSet) -- names.map(_.name).toSet

  /** A label indicating the type of an expression. */
  override def label: Label = LETREC

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(body) ++ bindings

  override def toString: String = {
    val bds = names.zip(bindings).map { case (name, binding) =>
      s"($name $binding)"
    }

    s"(letrec ${bds.mkString(" ")} $body)"
  }

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print(s"(letrec (", idn)

    names.lazyZip(bindings) map { (name, binding) =>
      printer.print("(")
      name.prettyPrint(printer)
      printer.print(" ")
      binding.prettyPrint(printer)
      printer.print(")")
    }

    printer.print(")")
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

case class ScFunctionAp(
    operator: ScExp,
    operands: List[ScExp],
    idn: Identity,
    annotation: Option[String] = None)
    extends ScAnnotated {

  override def annotatedExpression: ScExp = operands.head

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = operator.fv ++ operands.fv

  /** A label indicating the type of an expression. */
  override def label: Label = FUNCTION_AP

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(operator) ++ operands

  override def toString: String = s"($operator ${operands.map(_.toString).mkString(" ")})"

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print("(")
    operator.prettyPrint(printer)
    printer.print(" ")
    for (operand <- operands.init) {
      operand.prettyPrint(printer)
      printer.print(" ")
    }

    if (operands.lastOption.nonEmpty) {
      operands.last.prettyPrint(printer)
    }
    printer.print(")")
  }
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
    printer.print("(begin", idn)
    printer.newIndent()
    for (expression <- expressions) {
      expression.prettyPrint(printer)
      printer.newline()
    }
    printer.print(")")
    printer.exitIndent()
  }
}

case class ScSet(
    variable: ScIdentifier,
    value: ScExp,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = value.fv

  /** A label indicating the type of an expression. */
  override def label: Label = SET

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(value)

  override def toString: String = s"(set! $variable $value)"
}

trait ScAnnotated extends ScExp {
  val annotation: Option[String]
  def annotatedExpression: ScExp
}

case class ScMon(
    contract: ScExp,
    expression: ScExp,
    idn: Identity,
    annotation: Option[String] = None)
    extends ScAnnotated {

  override def annotatedExpression: ScExp = expression

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = contract.fv ++ expression.fv

  /** A label indicating the type of an expression. */
  override def label: Label = MONITOR

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(contract, expression)

  override def toString: String = s"(mon $contract $expression)"

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print("(mon ", idn)
    contract.prettyPrint(printer)
    printer.print(" ")
    expression.prettyPrint(printer)
    printer.print(")")
  }
}

case class ScCheck(
    contract: ScExp,
    returnValue: ScExp,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = contract.fv ++ returnValue.fv

  /** A label indicating the type of an expression. */
  override def label: Label = CHECK

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(contract, returnValue)
}

case class ScIf(
    condition: ScExp,
    consequent: ScExp,
    alternative: ScExp,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = condition.fv ++ consequent.fv ++ alternative.fv

  /** A label indicating the type of an expression. */
  override def label: Label = IF

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(condition, consequent, alternative)

  override def toString: String = s"(if $condition $consequent $alternative)"

  override def prettyPrint(printer: PrettyPrinter): Unit = {
    printer.print("(if", idn)
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

  override def toString: String =
    if (refinement.nonEmpty) s"(OPQ ${refinement.mkString(" ")}" else "OPQ"
}

/**
 * An assumption of the form:
 *
 * (assume (identifier assumption) expression)
 */
case class ScAssume(
    identifier: ScIdentifier,
    assumption: ScExp,
    expression: ScExp,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = expression.fv

  /** A label indicating the type of an expression. */
  override def label: Label = ASSUME

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(expression)
}

/**
 * A variable definition
 * (define x expression)
 */
case class ScDefine(
    variable: ScIdentifier,
    expression: ScExp,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = expression.fv

  /** A label indicating the type of an expression. */
  override def label: Label = DEFINE

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(expression)
}

/** A soft contract program */
case class ScProgram(expressions: List[ScExp], idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = expressions.map(_.fv).fold(Set())((a, b) => a ++ b)

  /** A label indicating the type of an expression. */
  override def label: Label = PROGRAM

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = expressions
}

/**
 * A function definition
 *
 * Syntax:
 * (define (name parameters ...) expressions ...)
 */
case class ScDefineFn(
    name: ScIdentifier,
    parameters: List[ScParam],
    expressions: ScBegin,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = expressions.fv -- parameters.map(_.name).toSet

  /** A label indicating the type of an expression. */
  override def label: Label = DEFINE_FN

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = expressions.subexpressions
}

/**
 * A function definition annotated with a contract
 *
 * Syntax:
 * (define (name parameters ...) contract expressions ...)
 */
case class ScDefineAnnotatedFn(
    name: ScIdentifier,
    parameters: List[ScParam],
    contract: ScExp,
    expressions: ScBegin,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = expressions.fv -- parameters.map(_.name).toSet

  /** A label indicating the type of an expression. */
  override def label: Label = DEFINE_ANNOTATED_FN

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = expressions.subexpressions
}

case class ScCons(
    car: ScExp,
    cdr: ScExp,
    idn: Identity)
    extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = car.fv ++ cdr.fv

  /** A label indicating the type of an expression. */
  override def label: Label = CONS

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(car, cdr)

  override def toString = s"(cons $car $cdr)"
}

case class ScCar(pai: ScExp, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = pai.fv

  /** A label indicating the type of an expression. */
  override def label: Label = CAR

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(pai)
}

case class ScCdr(pai: ScExp, idn: Identity) extends ScExp {

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = pai.fv

  /** A label indicating the type of an expression. */
  override def label: Label = CDR

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = List(pai)
}

case class ScProvideContracts(
    identifiers: List[ScIdentifier],
    contracts: List[ScExp],
    idn: Identity)
    extends ScExp {
  import maf.util.MonoidInstances._

  /** The set of free variables appearing in this expression. */
  override def fv: Set[String] = combineAllMap((c: ScExp) => c.fv)(contracts)

  /** A label indicating the type of an expression. */
  override def label: Label = PROVIDE_CONTRACT

  /** Returns the list of subexpressions of the given expression. */
  override def subexpressions: List[Expression] = contracts
}

abstract class ScTraverser[T: Monoid] {
  import maf.util.MonoidInstances._

  def traverse(exp: ScExp): T =
    combineAll(
      exp.subexpressions
        .filter {
          case _: ScExp => true
          case _        => false
        }
        .map(_.asInstanceOf[ScExp])
        .map(traverse)
    )
}

object ScTraverser {
  def traverse[T: Monoid](exp: ScExp)(f: ScExp => Either[T, T]): T = {
    val traverser = new ScTraverser[T] {
      override def traverse(exp: ScExp): T =
        f(exp) match {
          case Left(value) => value
          case Right(value) =>
            Monoid[T].append(value, super.traverse(exp))
        }
    }
    traverser.traverse(exp)
  }
}
