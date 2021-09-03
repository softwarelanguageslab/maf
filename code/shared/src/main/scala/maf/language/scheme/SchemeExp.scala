package maf.language.scheme

import maf.core._
import maf.language.change.ChangeExp
import maf.language.sexp._
import maf.language.scheme.ContractSchemeExp

/** Abstract syntax of Scheme programs */
sealed trait SchemeExp extends Expression

case object AND extends Label // And
case object BEG extends Label // Begin
case object DFF extends Label // Function definition
case object DFV extends Label // Variable definition
case object DVA extends Label // Variable argument definition
case object FNC extends Label // Function call (application)
case object IFF extends Label // If expression
case object LAM extends Label // Lambda expression
case object LET extends Label // Let expression
case object LTR extends Label // Letrec expression
case object LTS extends Label // Let* expression
case object NLT extends Label // Named let
case object ORR extends Label // Or
case object PAI extends Label // Pair
case object SET extends Label // Assignment
case object SPA extends Label // Spliced pair
case object VAL extends Label // Value
case object VAR extends Label // Variable
case object ASS extends Label // Assertion

/*
    case SchemeLambda(name, args, body, idn) =>
    case SchemeVarArgLambda(name, args, vararg, body, idn) =>
    case SchemeFuncall(f, args, idn) =>
    case SchemeIf(cond, cons, alt, idn) =>
    case SchemeLet(bindings, body, idn) =>
    case SchemeLetStar(bindings, body, idn) =>
    case SchemeLetrec(bindings, body, idn) =>
    case SchemeNamedLet(name, bindings, body, idn) =>
    case SchemeSet(variable, value, idn) =>
    case SchemeSetLex(variable, lexAddr, value, idn) =>
    case SchemeBegin(exps, idn) =>
    case SchemeAnd(exps, idn) =>
    case SchemeOr(exps, idn) =>
    case SchemeDefineVariable(name, value, idn) =>
    case SchemeDefineFunction(name, args, body, idn) =>
    case SchemeDefineVarArgFunction(name, args, vararg, body, idn) =>
    case SchemeVar(id) =>
    case SchemeVarLex(id, lexAdr) =>
    case SchemePair(car, cdr, idn) =>
    case SchemeSplicedPair(splice, cdr, idn) =>
    case SchemeValue(value, idn) =>
 */

/**
 * A lambda expression: (lambda (args...) body...) Not supported: "rest"-arguments, of the form (lambda arg body), or (lambda (arg1 . args) body...)
 */
sealed trait SchemeLambdaExp extends SchemeExp {
  // optionally, a lambda has a name
  def name: Option[String]
  // a lambda takes arguments, and has a non-empty body
  def args: List[Identifier]
  def body: List[SchemeExp]
  require(body.nonEmpty)
  // does the lambda support a variable number of arguments
  def varArgId: Option[Identifier]
  // can this lambda be called with a given number of arguments
  def check(argc: Int): Boolean =
    if (varArgId.isDefined) {
      argc >= args.length
    } else {
      argc == args.length
    }
  // a short name for this function
  def lambdaName: String = name.getOrElse(s"λ@${idn.pos}")
  // free variables
  lazy val fv: Set[String] = body.flatMap(_.fv).toSet -- args.map(_.name).toSet -- varArgId.map(id => Set(id.name)).getOrElse(Set[String]())
  // height
  override val height: Int = 1 + body.foldLeft(0)((mx, e) => mx.max(e.height))
  def annotation: Option[(String, String)] = body match {
    case SchemeVar(id) :: _ =>
      if (id.name.startsWith("@")) {
        id.name.split(':') match {
          case Array(name, value) => Some((name, value))
          case _                  => throw new Exception(s"Invalid annotation: $id")
        }
      } else {
        None
      }
    case _ => None
  }
  def label: Label = LAM
  def subexpressions: List[Expression] = args ::: body
  override def isomorphic(other: Expression): Boolean = super.isomorphic(other) && args.length == other.asInstanceOf[SchemeLambdaExp].args.length
  override def eql(other: Expression): Boolean = super.eql(other) && args.length == other.asInstanceOf[SchemeLambdaExp].args.length
}

case class SchemeLambda(
    name: Option[String],
    args: List[Identifier],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLambdaExp {
  override def toString: String = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(lambda ($a) $b)"
  }
  def varArgId: Option[Identifier] = None
}

case class SchemeVarArgLambda(
    name: Option[String],
    args: List[Identifier],
    vararg: Identifier,
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLambdaExp {
  override def toString: String = {
    val a = if (args.isEmpty) {
      vararg.toString
    } else {
      s"(${args.mkString(" ")} . $vararg)"
    }
    val b = body.mkString(" ")
    s"(lambda $a $b)"
  }
  def varArgId: Option[Identifier] = Some(vararg)
}

/** A function call: (f args...) */
case class SchemeFuncall(
    f: SchemeExp,
    args: List[SchemeExp],
    idn: Identity)
    extends SchemeExp {
  override def toString: String =
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  def fv: Set[String] = f.fv ++ args.flatMap(_.fv).toSet
  override val height: Int = 1 + args.foldLeft(0)((mx, a) => mx.max(a.height).max(f.height))
  val label: Label = FNC
  def subexpressions: List[Expression] = f :: args
}

/** An if statement: (if cond cons alt) If without alt clauses need to be encoded with an empty begin as alt clause */
case class SchemeIf(
    cond: SchemeExp,
    cons: SchemeExp,
    alt: SchemeExp,
    idn: Identity)
    extends SchemeExp {
  override def toString: String = s"(if $cond $cons $alt)"
  def fv: Set[String] = cond.fv ++ cons.fv ++ alt.fv
  override val height: Int = 1 + cond.height.max(cons.height).max(alt.height)
  val label: Label = IFF
  def subexpressions: List[Expression] = List(cond, cons, alt)
}

/** A let-like expression. */
sealed trait SchemeLettishExp extends SchemeExp {
  val bindings: List[(Identifier, SchemeExp)]
  val body: List[SchemeExp]
  val idn: Identity
  override val height: Int = 1 + bindings.foldLeft(0)((mx, b) => mx.max(b._2.height).max(body.foldLeft(0)((mx, e) => mx.max(e.height))))
  def subexpressions: List[Expression] = bindings.foldLeft(List[Expression]())((a, b) => b._2 :: b._1 :: a) ::: body
  override def isomorphic(other: Expression): Boolean = super.isomorphic(other) && body.length == other.asInstanceOf[SchemeLettishExp].body.length
  override def eql(other: Expression): Boolean = super.eql(other) && body.length == other.asInstanceOf[SchemeLettishExp].body.length
}

/** Let-bindings: (let ((v1 e1) ...) body...) */
case class SchemeLet(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp {
  override def toString: String = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let ($bi) $bo)"
  }
  def fv: Set[String] =
    bindings.map(_._2).flatMap(_.fv).toSet ++ (body.flatMap(_.fv).toSet -- bindings
      .map(_._1.name)
      .toSet)
  val label: Label = LET
}

/** Let*-bindings: (let* ((v1 e1) ...) body...) */
case class SchemeLetStar(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp {
  override def toString: String = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let* ($bi) $bo)"
  }
  def fv: Set[String] =
    bindings
      .foldLeft((Set.empty[String] /* bound variables */, Set.empty[String] /* free variables */ ))((acc, binding) =>
        binding match {
          case (id, e) => (acc._1 + id.name, acc._2 ++ (e.fv -- acc._1))
        }
      )
      ._2 ++ (body.flatMap(_.fv).toSet -- bindings.map(_._1.name).toSet)
  val label: Label = LTS
}

/** Letrec-bindings: (letrec ((v1 e1) ...) body...) */
case class SchemeLetrec(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp {
  override def toString: String = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(letrec ($bi) $bo)"
  }
  def fv: Set[String] =
    (bindings.map(_._2).flatMap(_.fv).toSet ++ body.flatMap(_.fv).toSet) -- bindings
      .map(_._1.name)
      .toSet
  val label: Label = LTR
}

/** Named-let: (let name ((v1 e1) ...) body...) */
object SchemeNamedLet {
  def apply(name: Identifier, bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], idn: Identity): SchemeExp = {
    val (prs, ags) = bindings.unzip
    val fnDef = 
      SchemeLetrec(List((name, SchemeLambda(Some(name.name), prs, body, idn))),
                   List((SchemeVar(Identifier(name.name, idn)))),
                   idn)
    SchemeFuncall(fnDef, ags, idn)
  }
}

/** A set! expression: (set! variable value) */
sealed trait SchemeSetExp extends SchemeExp {
  val variable: Identifier
  val value: SchemeExp
  def fv: Set[String] = value.fv + variable.name
  override val height: Int = 1 + value.height
  val label: Label = SET
  def subexpressions: List[Expression] = List(variable, value)
}

case class SchemeSet(
    variable: Identifier,
    value: SchemeExp,
    idn: Identity)
    extends SchemeSetExp {
  override def toString: String = s"(set! $variable $value)"
}

case class SchemeSetLex(
    variable: Identifier,
    lexAddr: LexicalRef,
    value: SchemeExp,
    idn: Identity)
    extends SchemeSetExp {
  override def toString = s"(set! $lexAddr $value)"
}

/** A begin clause: (begin body...) */
case class SchemeBegin(exps: List[SchemeExp], idn: Identity) extends SchemeExp {
  override def toString: String = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
  def fv: Set[String] = exps.flatMap(_.fv).toSet
  override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
  val label: Label = BEG
  def subexpressions: List[Expression] = exps
}

/** Used to create a begin if there are multiple statements, and a single exp if there is only one */
object SchemeBody {
  def apply(exps: List[SchemeExp]): SchemeExp = exps match {
    case Nil        => SchemeValue(Value.Boolean(false), NoCodeIdentity) /* undefined */
    case exp :: Nil => exp
    case exp :: _   => SchemeBegin(exps, exp.idn)
  }
}

/** A cond expression: (cond (test1 body1...) ...). Desugared according to R5RS. */
object SchemeCond {
  def apply(clauses: List[(SchemeExp, List[SchemeExp])], idn: Identity): SchemeExp =
    if (clauses.isEmpty) {
      throw new Exception(s"Invalid Scheme cond without clauses ($idn)")
    } else {
      clauses.foldRight[SchemeExp](SchemeValue(Value.Boolean(false /* undefined */ ), NoCodeIdentity))((clause, acc) =>
        clause match {
          case (SchemeValue(Value.Boolean(true), _), body) => SchemeBody(body)
          case (cond, Nil)                                 =>
            /* Body is empty. R5RS states that "If the selected clause contains only the
             * test and no expressions ,then the value of the test is returned
             * as the result" */
            val id = Identifier("__cond-empty-body", cond.idn)
            SchemeLet(
              List((id, cond)),
              List(SchemeIf(SchemeVar(id), SchemeVar(id), acc, cond.idn)),
              cond.idn
            )
          case (cond, body) => SchemeIf(cond, SchemeBody(body), acc, cond.idn)
        }
      )
    }
}

/** A when expression: (when pred body ...) Desugared into an if-expression. */
object SchemeWhen {
  def apply(
      pred: SchemeExp,
      body: List[SchemeExp],
      idn: Identity
    ): SchemeExp =
    SchemeIf(pred, SchemeBody(body), SchemeValue(Value.Boolean(false), idn), idn)
}

/** An unless expression: (unless pred body ...) Desugared into an if-expression. */
object SchemeUnless {
  def apply(
      pred: SchemeExp,
      body: List[SchemeExp],
      idn: Identity
    ): SchemeExp =
    SchemeIf(pred, SchemeValue(Value.Boolean(false), idn), SchemeBody(body), idn)
}

/** A case expression: (case key ((vals1...) body1...) ... (else default...)). Desugared according to R5RS. */
object SchemeCase {
  def apply(
      key: SchemeExp,
      clauses: List[(List[SchemeValue], List[SchemeExp])],
      default: List[SchemeExp],
      idn: Identity
    ): SchemeExp = key match {
    case _: SchemeVar | _: SchemeValue =>
      /** Atomic key */
      val eqv = SchemeVar(Identifier("eq?", NoCodeIdentity)) /* TODO: should be eqv? instead of eq? */
      clauses.foldRight[SchemeExp](SchemeBody(default))((clause, acc) =>
        /**
         * In R5RS, the condition is desugared into a (memv key '(atoms ...)) call. This would mean we would have to construct a list and go through
         * it, which would badly impact precision. Hence, we instead explicitly do a big-or with eq?
         */
        SchemeIf(
          SchemeOr(
            clause._1.map(atom => SchemeFuncall(eqv, List(key, atom), atom.idn)),
            idn
          ),
          SchemeBody(clause._2),
          acc,
          idn
        )
      )
    case _ =>
      /** Non-atomic key, let-bind it */
      val id = Identifier("__case-atom-key", key.idn)
      SchemeLet(List((id, key)), List(SchemeCase(SchemeVar(id), clauses, default, idn)), key.idn)
  }
}

/** An and expression: (and exps...) */
case class SchemeAnd(exps: List[SchemeExp], idn: Identity) extends SchemeExp {
  override def toString: String = {
    val e = exps.mkString(" ")
    s"(and $e)"
  }
  def fv: Set[String] = exps.flatMap(_.fv).toSet
  override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
  val label: Label = AND
  def subexpressions: List[Expression] = exps
}

/** An or expression: (or exps...) */
case class SchemeOr(exps: List[SchemeExp], idn: Identity) extends SchemeExp {
  override def toString: String = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
  def fv: Set[String] = exps.flatMap(_.fv).toSet
  override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
  val label: Label = ORR
  def subexpressions: List[Expression] = exps
}

/** A variable definition: (define name value) */
case class SchemeDefineVariable(
    name: Identifier,
    value: SchemeExp,
    idn: Identity)
    extends SchemeExp {
  override def toString: String = s"(define $name $value)"
  def fv: Set[String] = value.fv
  override val height: Int = 1 + value.height
  val label: Label = DFV
  def subexpressions: List[Expression] = List(name, value)
}

/** A function definition: (define (name args...) body...) */
sealed trait SchemeDefineFunctionExp extends SchemeExp {
  val name: Identifier
  val args: List[Identifier]
  val body: List[SchemeExp]
  val idn: Identity
  def fv: Set[String] = body.flatMap(_.fv).toSet -- (args.map(_.name).toSet + name.name)
  def subexpressions: List[Expression] = name :: args ::: body
  def annotation: Option[(String, String)] = body match {
    case SchemeVar(id) :: _ =>
      if (id.name.startsWith("@")) {
        id.name.split(':') match {
          case Array(name, value) => Some((name, value))
          case _                  => throw new Exception(s"Invalid annotation: $id")
        }
      } else {
        None
      }
    case _ => None
  }
  override def isomorphic(other: Expression): Boolean =
    super.isomorphic(other) && args.length == other.asInstanceOf[SchemeDefineFunctionExp].args.length
  override def eql(other: Expression): Boolean = super.eql(other) && args.length == other.asInstanceOf[SchemeDefineFunctionExp].args.length
}

case class SchemeDefineFunction(
    name: Identifier,
    args: List[Identifier],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeDefineFunctionExp {
  override def toString: String = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
  override val height: Int = 1 + body.foldLeft(0)((mx, e) => mx.max(e.height))
  val label: Label = DFF
}

case class SchemeDefineVarArgFunction(
    name: Identifier,
    args: List[Identifier],
    vararg: Identifier,
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeDefineFunctionExp {
  override def toString: String = {
    val a = s"${args.mkString(" ")} . $vararg"
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
  override def fv: Set[String] = super.fv - vararg.name
  override val height: Int = 1 + body.foldLeft(0)((mx, e) => mx.max(e.height))
  val label: Label = DVA
  override def subexpressions: List[Expression] = vararg :: super.subexpressions
}

/**
 * Do notation: (do ((<variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...). Desugared according to R5RS, i.e., a do becomes:
 * (letrec ((loop (lambda (variable1 variable2 ...) (if <test> <finals> (begin <body> (loop <step1> ...)))))))
 */
object SchemeDo {
  def apply(
      vars: List[(Identifier, SchemeExp, Option[SchemeExp])],
      test: SchemeExp,
      finals: List[SchemeExp],
      commands: List[SchemeExp],
      idn: Identity
    ): SchemeExp = {
    val loopIdName = "__do_loop"
    val loopId = Identifier(loopIdName, idn)
    val annot = commands.take(1) match {
      case (exp @ SchemeVar(Identifier(annot, _))) :: _ if annot.startsWith("@") =>
        Some(exp)
      case _ =>
        None
    }
    val commandsWithoutAnnot = if (annot.isDefined) { commands.drop(1) }
    else { commands }
    SchemeLetrec(
      List(
        (
          loopId,
          SchemeLambda(
            Some(loopIdName),
            vars.map(_._1),
            (if (annot.isDefined) { annot.toList }
             else { List[SchemeExp]() }) ++
              List(
                SchemeIf(
                  test,
                  SchemeBody(finals),
                  SchemeBody(
                    commandsWithoutAnnot :::
                      List(
                        SchemeFuncall(SchemeVar(loopId),
                                      vars.map({
                                        case (_, _, Some(step)) => step
                                        case (id, _, None)      => SchemeVar(id)
                                      }),
                                      idn
                        )
                      )
                  ),
                  idn
                )
              ),
            idn
          )
        )
      ),
      List(SchemeFuncall(SchemeVar(loopId), vars.map(_._2), idn)),
      idn
    )
  }
}

/** An identifier: name */
sealed trait SchemeVarExp extends SchemeExp {
  val id: Identifier
  override val height: Int = 1
  val idn: Identity = id.idn
  def fv: Set[String] = Set(id.name)
  val label: Label = VAR
  def subexpressions: List[Expression] = List(id)
}

case class SchemeVar(id: Identifier) extends SchemeVarExp {
  override def toString: String = id.name
}

case class SchemeVarLex(id: Identifier, lexAddr: LexicalRef) extends SchemeVarExp {
  override def toString: String = lexAddr.toString
}

case class SchemePair(
    car: SchemeExp,
    cdr: SchemeExp,
    idn: Identity)
    extends SchemeExp {
  override def toString: String = s"`($contentToString)"
  private def contentToString: String = cdr match {
    case SchemeValue(Value.Nil, _) => printElement(car)
    case pair: SchemePair          => s"${printElement(car)} ${pair.contentToString}"
    case _                         => s"${printElement(car)} . ${printElement(cdr)}"
  }
  private def printElement(elm: SchemeExp): String = elm match {
    case SchemeValue(Value.Symbol(sym), _) => sym
    case SchemeValue(v, _)                 => v.toString
    case pair: SchemePair                  => s"(${pair.contentToString})"
    case _                                 => s",$elm"
  }
  def fv: Set[String] = car.fv ++ cdr.fv
  val label: Label = PAI
  def subexpressions: List[Expression] = List(car, cdr)
}

case class SchemeSplicedPair(
    splice: SchemeExp,
    cdr: SchemeExp,
    idn: Identity)
    extends SchemeExp {
  override def toString: String = s"`(,@$splice . ,$cdr)"
  def fv: Set[String] = splice.fv ++ cdr.fv
  val label: Label = SPA
  def subexpressions: List[Expression] = List(splice, cdr)
}

/** A literal value (number, symbol, string, ...) */
case class SchemeValue(value: Value, idn: Identity) extends SchemeExp {
  override def toString: String = value.toString
  def fv: Set[String] = Set()
  override val height: Int = 1
  val label: Label = VAL
  def subexpressions: List[Expression] = List()
  override lazy val hash: Int = (label, value).hashCode()
}

/** An assertion (assert <exp>) */
case class SchemeAssert(exp: SchemeExp, idn: Identity) extends SchemeExp {
  override def toString: String = s"(assert $exp)"
  def fv: Set[String] = exp.fv
  val label: Label = ASS
  def subexpressions: List[Expression] = List(exp)
}

/** Creates explicit (mutable) reference */
object SchemeRef {
    def apply(exp: SchemeExp, idn: Identity) =
        SchemeFuncall(SchemeVar(Identifier("__toplevel_cons", idn)),
                      List(SchemeValue(Value.Symbol("ref"), idn), exp),
                      idn)
}

/** Dereferences an explicit (mutable) reference */
object SchemeDeref {
    def apply(ref: SchemeExp, idn: Identity) = 
        SchemeFuncall(SchemeVar(Identifier("__toplevel_cdr", idn)),
                      List(ref),
                      idn)
}

/** Updates an explicit (mutable) reference */
object SchemeSetRef {
    def apply(ref: SchemeExp, exp: SchemeExp, idn: Identity) = 
        SchemeFuncall(SchemeVar(Identifier("__toplevel_set-cdr!", idn)),
                      List(ref, exp),
                      idn)
}

/** A code change in a Scheme program. */
case class SchemeCodeChange(
    old: SchemeExp,
    nw: SchemeExp,
    idn: Identity)
    extends ChangeExp[SchemeExp]
       with SchemeExp

trait CSchemeExp extends SchemeExp

case object FRK extends Label // Fork
case object JOI extends Label // Join

/** Fork a thread with an expression to evaluate. */
case class CSchemeFork(body: SchemeExp, idn: Identity) extends CSchemeExp {
  def fv: Set[String] = body.fv
  def label: Label = FRK
  def subexpressions: List[Expression] = List(body)
  override val height: Int = body.height + 1
}

/** Join a thread, given an expression that should evaluate to a TID. */
case class CSchemeJoin(tExp: SchemeExp, idn: Identity) extends CSchemeExp {
  def fv: Set[String] = tExp.fv
  def label: Label = JOI
  def subexpressions: List[Expression] = List(tExp)
  override val height: Int = tExp.height + 1
}

trait ContractSchemeExp extends SchemeExp

case object DPC extends Label // Dependent contract
case object FLC extends Label // Flat contract
case object MON extends Label // Monitor
case object DFC extends Label // define/contract

case class ContractSchemeDepContract(
    domains: List[SchemeExp],
    rangeMaker: SchemeExp,
    idn: Identity)
    extends ContractSchemeExp {
  def fv: Set[String] = domains.flatMap(_.fv).toSet ++ rangeMaker.fv
  def label: Label = DPC
  def subexpressions: List[Expression] = rangeMaker :: domains
  override def toString: String = s"(~>d ${domains.map(_.toString).mkString(" ")} $rangeMaker)"
}

case class ContractSchemeFlatContract(
    expression: SchemeExp,
    idn: Identity)
    extends ContractSchemeExp {
  def fv: Set[String] = expression.fv
  def label: Label = FLC
  def subexpressions: List[Expression] = List(expression)
  override def toString: String = s"(flat $expression)"
}

case class ContractSchemeMon(
    contract: SchemeExp,
    expression: SchemeExp,
    idn: Identity)
    extends ContractSchemeExp {
  def fv: Set[String] = contract.fv ++ expression.fv
  def label: Label = MON
  def subexpressions: List[Expression] = List(contract, expression)
  override def toString: String = s"(mon $contract $expression)"
}

case class ContractSchemeDefineContract(
  name: Identifier, 
  params: List[Identifier],
  contract: SchemeExp, 
  expression: SchemeExp,
  idn: Identity
) extends ContractSchemeExp {
  def fv: Set[String] = contract.fv ++ (expression.fv -- params.map(_.name))
  def label: Label = DFC
  def subexpressions: List[Expression] = List(contract, expression)
  override def toString: String = s"(define/contract ($name ${params.mkString(" ")}) $expression)"
}
