package maf.language.scheme

import maf.core._
import maf.language.change.ChangeExp
import maf.language.sexp._
import maf.language.scheme.ContractSchemeExp
import maf.language.ContractScheme.MatchPat
import Label.*

/** Abstract syntax of Scheme programs */
sealed trait SchemeExp extends Expression:
    def prettyString(indent: Int = 0): String = toString()
    def nextIndent(current: Int): Int = current + 3

/*
    case SchemeLambda(name, args, body, ann, idn) =>
    case SchemeVarArgLambda(name, args, vararg, body, ann, idn) =>
    case SchemeFuncall(f, args, idn) =>
    case SchemeIf(cond, cons, alt, idn) =>
    case SchemeLet(bindings, body, idn) =>
    case SchemeLetStar(bindings, body, idn) =>
    case SchemeLetrec(bindings, body, idn) =>
    case SchemeSet(variable, value, idn) =>
    case SchemeSetLex(variable, lexAddr, value, idn) =>
    case SchemeBegin(exps, idn) =>
    case SchemeDefineVariable(name, value, idn) =>
    case SchemeDefineFunction(name, args, body, idn) =>
    case SchemeDefineVarArgFunction(name, args, vararg, body, idn) =>
    case SchemeVar(id) =>
    case SchemeVarLex(id, lexAdr) =>
    case SchemeValue(value, idn) =>
 */

/**
 * A lambda expression: (lambda (args...) body...) Not supported: "rest"-arguments, of the form (lambda arg body), or (lambda (arg1 . args) body...)
 */
sealed trait SchemeLambdaExp extends SchemeExp:
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
      if varArgId.isDefined then argc >= args.length
      else argc == args.length
    // a short name for this function
    def lambdaName: String = name.getOrElse(s"λ@${idn.pos}")
    // free variables
    lazy val fv: Set[String] = SchemeBody.fv(body) -- args.map(_.name).toSet -- varArgId.map(id => Set(id.name)).getOrElse(Set[String]())
    // height
    override val height: Int = 1 + body.foldLeft(0)((mx, e) => mx.max(e.height))
    def annotation: Option[(String, String)]
    def label: Label = LAM
    def subexpressions: List[Expression] = args ::: body
    override def isomorphic(other: Expression): Boolean = super.isomorphic(other) && args.length == other.asInstanceOf[SchemeLambdaExp].args.length
    override def eql(other: Expression): Boolean = super.eql(other) && args.length == other.asInstanceOf[SchemeLambdaExp].args.length

case class SchemeLambda(
    name: Option[String],
    args: List[Identifier],
    body: List[SchemeExp],
    annotation: Option[(String, String)],
    idn: Identity)
    extends SchemeLambdaExp:
    override def toString: String =
        val a = args.mkString(" ")
        val b = body.mkString(" ")
        s"(lambda ($a) $b)"
    def varArgId: Option[Identifier] = None
    override def prettyString(indent: Int): String =
      s"(lambda (${args.mkString(" ")})\n${body.map(" " * nextIndent(indent) ++ _.prettyString(nextIndent(indent))).mkString("\n")})"

case class SchemeVarArgLambda(
    name: Option[String],
    args: List[Identifier],
    vararg: Identifier,
    body: List[SchemeExp],
    annotation: Option[(String, String)],
    idn: Identity)
    extends SchemeLambdaExp:
    override def toString: String =
        val a =
          if args.isEmpty then vararg.toString
          else s"(${args.mkString(" ")} . $vararg)"
        val b = body.mkString(" ")
        s"(lambda $a $b)"
    def varArgId: Option[Identifier] = Some(vararg)
    override def prettyString(indent: Int): String =
        val a =
          if args.isEmpty then vararg.toString
          else s"(${args.mkString(" ")} . $vararg)"
        s"(lambda $a\n${body.map(" " * nextIndent(indent) ++ _.prettyString(nextIndent(indent))).mkString("\n")})"

/** A function call: (f args...) */
case class SchemeFuncall(
    f: SchemeExp,
    args: List[SchemeExp],
    idn: Identity)
    extends SchemeExp:
    override def toString: String =
      if args.isEmpty then s"($f)"
      else
          val a = args.mkString(" ")
          s"($f $a)"
    def fv: Set[String] = f.fv ++ args.flatMap(_.fv).toSet
    override val height: Int = 1 + args.foldLeft(0)((mx, a) => mx.max(a.height).max(f.height))
    val label: Label = FNC
    def subexpressions: List[Expression] = f :: args
    override def prettyString(indent: Int): String =
      if this.toString.length < 100 then this.toString
      else
          val fnString = f.prettyString(indent)
          val argsString =
            if args.isEmpty then "" else "\n" + args.map(" " * nextIndent(indent) ++ _.prettyString(nextIndent(indent))).mkString("\n")
          s"(${if fnString.split("\n").nn.length <= 3 then f.toString else fnString}$argsString)"

/** An if statement: (if cond cons alt) If without alt clauses need to be encoded with an empty begin as alt clause */
case class SchemeIf(
    cond: SchemeExp,
    cons: SchemeExp,
    alt: SchemeExp,
    idn: Identity)
    extends SchemeExp:
    override def toString: String = s"(if $cond $cons $alt)"
    def fv: Set[String] = cond.fv ++ cons.fv ++ alt.fv
    override val height: Int = 1 + cond.height.max(cons.height).max(alt.height)
    val label: Label = IFF
    def subexpressions: List[Expression] = List(cond, cons, alt)
    override def prettyString(indent: Int): String =
      if this.toString.size < 50 then this.toString
      else
          s"(if $cond\n${" " * nextIndent(indent)}${cons.prettyString(nextIndent(indent))}\n${" " * nextIndent(indent)}${alt.prettyString(nextIndent(indent))})"

/** A let-like expression. */
sealed trait SchemeLettishExp extends SchemeExp:
    val bindings: List[(Identifier, SchemeExp)]
    val body: List[SchemeExp]
    val idn: Identity
    override val height: Int = 1 + bindings.foldLeft(0)((mx, b) => mx.max(b._2.height).max(body.foldLeft(0)((mx, e) => mx.max(e.height))))
    def subexpressions: List[Expression] = bindings.foldLeft(List[Expression]())((a, b) => b._2 :: b._1 :: a) ::: body
    override def isomorphic(other: Expression): Boolean = super.isomorphic(other) && body.length == other.asInstanceOf[SchemeLettishExp].body.length
    override def eql(other: Expression): Boolean = super.eql(other) && body.length == other.asInstanceOf[SchemeLettishExp].body.length
    def letName: String
    override def prettyString(indent: Int): String =
        val id = letName
        val shiftB = indent + id.toString.length + 3
        val bi = bindings.map({ case (name, exp) => s"($name ${exp.prettyString(shiftB + name.toString.length + 1)})" })
        val first = bi.headOption.map(_.toString).getOrElse("") // Apparently bi can be empty.
        val rest = if bi.isEmpty || bi.tail.isEmpty then "" else "\n" ++ bi.tail.map(s => (" " * shiftB) + s).mkString("\n")
        val bo = body.map(" " * nextIndent(indent) ++ _.prettyString(nextIndent(indent))).mkString("\n")
        s"($id (${first}${rest})\n$bo)"

/** Let-bindings: (let ((v1 e1) ...) body...) */
case class SchemeLet(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp:
    override def toString: String =
        val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
        val bo = body.mkString(" ")
        s"(let ($bi) $bo)"
    def fv: Set[String] =
      bindings.map(_._2).flatMap(_.fv).toSet ++ (SchemeBody.fv(body) -- bindings.map(_._1.name).toSet)
    val label: Label = LET
    def letName: String = "let"

/** Let*-bindings: (let* ((v1 e1) ...) body...) */
case class SchemeLetStar(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp:
    override def toString: String =
        val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
        val bo = body.mkString(" ")
        s"(let* ($bi) $bo)"
    def fv: Set[String] =
      bindings
        .foldLeft((Set.empty[String] /* bound variables */, Set.empty[String] /* free variables */ ))((acc, binding) =>
          binding match {
            case (id, e) => (acc._1 + id.name, acc._2 ++ (e.fv -- acc._1))
          }
        )
        ._2 ++ (SchemeBody.fv(body) -- bindings.map(_._1.name).toSet)
    val label: Label = LTS
    def letName: String = "let*"

/** Letrec-bindings: (letrec* ((v1 e1) ...) body...) */
case class SchemeLetrec(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp:
    override def toString: String =
        val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
        val bo = body.mkString(" ")
        s"(letrec ($bi) $bo)"
    def fv: Set[String] =
      (bindings.map(_._2).flatMap(_.fv).toSet ++ SchemeBody.fv(body)) -- bindings
        .map(_._1.name)
        .toSet
    val label: Label = LTR
    def letName: String = "letrec"
    if bindings.size > bindings.map(_._1.name).toSet.size then
        throw new Exception(
          s"Illegal letrec: duplicate definitions (${idn.pos}): ${bindings.map(_._1.name).groupBy(name => name).view.mapValues(_.size).toList.filter(_._2 > 1).sorted.map(p => s"${p._1} (${p._2})").mkString("{", ", ", "}")}."
        )

/** Named-let: (let name ((v1 e1) ...) body...) */
object SchemeNamedLet:
    def apply(
        name: Identifier,
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        annotation: Option[(String, String)],
        idn: Identity
      ): SchemeExp =
        val (prs, ags) = bindings.unzip
        val fnDef =
          SchemeLetrec(List((name, SchemeLambda(Some(name.name), prs, body, annotation, idn))), List(SchemeVar(Identifier(name.name, idn))), idn)
        SchemeFuncall(fnDef, ags, idn)

/** A set! expression: (set! variable value) */
sealed trait SchemeSetExp extends SchemeExp:
    val variable: Identifier
    val value: SchemeExp
    def fv: Set[String] = value.fv + variable.name
    override val height: Int = 1 + value.height
    val label: Label = SET
    def subexpressions: List[Expression] = List(variable, value)
    override def prettyString(indent: Int): String =
        val ind = indent + variable.toString.length + 4
        s"(set! $variable ${value.prettyString(ind)})"

case class SchemeSet(
    variable: Identifier,
    value: SchemeExp,
    idn: Identity)
    extends SchemeSetExp:
    override def toString: String = s"(set! $variable $value)"

case class SchemeSetLex(
    variable: Identifier,
    lexAddr: LexicalRef,
    value: SchemeExp,
    idn: Identity)
    extends SchemeSetExp:
    override def toString = s"(set! $variable $value)"

/** A begin clause: (begin body...) */
case class SchemeBegin(exps: List[SchemeExp], idn: Identity) extends SchemeExp:
    override def toString: String =
        val body = exps.mkString(" ")
        s"(begin $body)"
    def fv: Set[String] = exps.flatMap(_.fv).toSet
    override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
    val label: Label = BEG
    def subexpressions: List[Expression] = exps
    override def prettyString(indent: Int): String =
      s"(begin\n${exps.map(" " * nextIndent(indent) ++ _.prettyString(nextIndent(indent))).mkString("\n")})"

/** Used to create a begin if there are multiple statements, and a single exp if there is only one */
object SchemeBody:
    def apply(exps: List[SchemeExp]): SchemeExp = exps match
        case Nil        => SchemeValue(Value.Boolean(false), NoCodeIdentity) /* undefined */
        case exp :: Nil => exp
        case exp :: _   => SchemeBegin(exps, exp.idn)
    def defs(bdy: List[SchemeExp]): List[Identifier] = defs(bdy, Nil)
    def defs(bdy: List[SchemeExp], cur: List[Identifier]): List[Identifier] =
      bdy.foldLeft(cur)((acc, exp) =>
        exp match {
          case SchemeBegin(eps, _)             => SchemeBody.defs(eps, acc)
          case SchemeDefineVariable(nam, _, _) => nam :: acc
          case _                               => acc
        }
      )
    def fv(bdy: List[SchemeExp]): Set[String] =
      bdy.flatMap(_.fv).toSet -- defs(bdy).map(_.name)

/** A cond expression: (cond (test1 body1...) ...). Desugared according to R5RS. */
object SchemeCond:
    def apply(clauses: List[(SchemeExp, List[SchemeExp])], idn: Identity): SchemeExp =
      if clauses.isEmpty then throw new Exception(s"Invalid Scheme cond without clauses ($idn)")
      else
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

/** A when expression: (when pred body ...) Desugared into an if-expression. */
object SchemeWhen:
    def apply(
        pred: SchemeExp,
        body: List[SchemeExp],
        idn: Identity
      ): SchemeExp =
      SchemeIf(pred, SchemeBody(body), SchemeValue(Value.Boolean(false), idn), idn)

/** An unless expression: (unless pred body ...) Desugared into an if-expression. */
object SchemeUnless:
    def apply(
        pred: SchemeExp,
        body: List[SchemeExp],
        idn: Identity
      ): SchemeExp =
      SchemeIf(pred, SchemeValue(Value.Boolean(false), idn), SchemeBody(body), idn)

/** A case expression: (case key ((vals1...) body1...) ... (else default...)). Desugared according to R5RS. */
object SchemeCase:
    def apply(
        key: SchemeExp,
        clauses: List[(List[SchemeValue], List[SchemeExp])],
        default: List[SchemeExp],
        idn: Identity
      ): SchemeExp = key match
        case _: SchemeVar | _: SchemeValue =>
          /** Atomic key */
          val eqv = SchemeVar(Identifier("eq?", NoCodeIdentity)) /* TODO: should be eqv? instead of eq? */
          clauses.foldRight[SchemeExp](SchemeBody(default))((clause, acc) =>
            /**
             * In R5RS, the condition is desugared into a (memv key '(atoms ...)) call. This would mean we would have to construct a list and go
             * through it, which would badly impact precision. Hence, we instead explicitly do a big-or with eq?
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

/** An and expression: (and exps...) */
object SchemeAnd:
    def apply(eps: List[SchemeExp], idn: Identity): SchemeExp = eps match
        case Nil        => SchemeValue(Value.Boolean(true), idn)
        case exp :: Nil => exp
        case exp :: rst => SchemeIf(exp, SchemeAnd(rst, idn), SchemeValue(Value.Boolean(false), idn), idn)

/** An or expression: (or exps...) */
object SchemeOr:
    def apply(eps: List[SchemeExp], idn: Identity): SchemeExp = eps match
        case Nil        => SchemeValue(Value.Boolean(false), idn)
        case exp :: Nil => exp
        case exp :: rst =>
          val tmp = "__or_res"
          SchemeLet(
            List((Identifier(tmp, exp.idn), exp)),
            List((SchemeIf(SchemeVar(Identifier(tmp, Identity.none)), SchemeVar(Identifier(tmp, Identity.none)), SchemeOr(rst, idn), idn))),
            idn
          )

/** A variable definition: (define name value) */
case class SchemeDefineVariable(
    name: Identifier,
    value: SchemeExp,
    idn: Identity)
    extends SchemeExp:
    override def toString: String = s"(define $name $value)"
    def fv: Set[String] = value.fv - name.name
    override val height: Int = 1 + value.height
    val label: Label = DFV
    def subexpressions: List[Expression] = List(name, value)
    override def prettyString(indent: Int): String = s"(define $name ${value.prettyString(nextIndent(indent))})"

/** Function definition of the form (define (f arg ...) body) */
object SchemeDefineFunction:
    def apply(name: Identifier, args: List[Identifier], body: List[SchemeExp], annotation: Option[(String, String)], idn: Identity): SchemeExp =
      SchemeDefineVariable(name, SchemeLambda(Some(name.name), args, body, annotation, idn), idn)

/** Function definition with varargs of the form (define (f arg . vararg ...) body) */
object SchemeDefineVarArgFunction:
    def apply(
        name: Identifier,
        args: List[Identifier],
        vararg: Identifier,
        body: List[SchemeExp],
        annotation: Option[(String, String)],
        idn: Identity
      ): SchemeExp =
      SchemeDefineVariable(name, SchemeVarArgLambda(Some(name.name), args, vararg, body, annotation, idn), idn)

/**
 * Do notation: (do ((<variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...). Desugared according to R5RS, i.e., a do becomes:
 * (letrec ((loop (lambda (variable1 variable2 ...) (if <test> <finals> (begin <body> (loop <step1> ...)))))))
 */
object SchemeDo:
    def apply(
        vars: List[(Identifier, SchemeExp, Option[SchemeExp])],
        test: SchemeExp,
        finals: List[SchemeExp],
        commands: List[SchemeExp],
        idn: Identity
      ): SchemeExp =
        val loopIdName = "__do_loop"
        val loopId = Identifier(loopIdName, idn)
        val annot = commands.take(1) match
            case SchemeVar(id) :: _ if id.name.startsWith("@") =>
              id.name.split(':') match
                  case Array(name, value) => Some((name, value))
                  case _                  => throw new Exception(s"Invalid annotation: $id")
            case _ => None
        val commandsWithoutAnnot = if annot.isDefined then { commands.drop(1) }
        else { commands }
        SchemeLetrec(
          List(
            (
              loopId,
              SchemeLambda(
                Some(loopIdName),
                vars.map(_._1),
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
                annot,
                idn
              )
            )
          ),
          List(SchemeFuncall(SchemeVar(loopId), vars.map(_._2), idn)),
          idn
        )

/** An identifier: name */
sealed trait SchemeVarExp extends SchemeExp:
    val id: Identifier
    override val height: Int = 1
    val idn: Identity = id.idn
    def fv: Set[String] = Set(id.name)
    val label: Label = VAR
    def subexpressions: List[Expression] = List(id)

case class SchemeVar(id: Identifier) extends SchemeVarExp:
    override def toString: String = id.name

case class SchemeVarLex(id: Identifier, lexAddr: LexicalRef) extends SchemeVarExp:
    override def toString: String = id.name

object SchemePair:
    def apply(car: SchemeExp, cdr: SchemeExp, idn: Identity): SchemeExp =
      SchemeFuncall(SchemeVar(Identifier("__toplevel_cons", idn)), List(car, cdr), idn)

object SchemeSplicedPair:
    def apply(splice: SchemeExp, cdr: SchemeExp, idn: Identity): SchemeExp =
      SchemeFuncall(SchemeVar(Identifier("__toplevel_append", idn)), List(splice, cdr), idn)

/** A   value (number, symbol, string, ...) */
case class SchemeValue(value: Value, idn: Identity) extends SchemeExp:
    override def toString: String = value.toString
    def fv: Set[String] = Set()
    override val height: Int = 1
    val label: Label = VAL
    def subexpressions: List[Expression] = List()
    override lazy val hash: Int = (label, value).hashCode()

/** An assertion (assert <exp>) */
case class SchemeAssert(exp: SchemeExp, idn: Identity) extends SchemeExp:
    override def toString: String = s"(assert $exp)"
    def fv: Set[String] = exp.fv
    val label: Label = ASS
    def subexpressions: List[Expression] = List(exp)

/** Creates explicit (mutable) reference */
object SchemeRef:
    def apply(exp: SchemeExp, idn: Identity) =
      SchemeFuncall(SchemeVar(Identifier("__toplevel_cons", idn)), List(SchemeValue(Value.Symbol("ref"), idn), exp), idn)

/** Dereferences an explicit (mutable) reference */
object SchemeDeref:
    def apply(ref: SchemeExp, idn: Identity) =
      SchemeFuncall(SchemeVar(Identifier("__toplevel_cdr", idn)), List(ref), idn)

/** Updates an explicit (mutable) reference */
object SchemeSetRef:
    def apply(ref: SchemeExp, exp: SchemeExp, idn: Identity) =
      SchemeFuncall(SchemeVar(Identifier("__toplevel_set-cdr!", idn)), List(ref, exp), idn)

/** A code change in a Scheme program. */
case class SchemeCodeChange(old: SchemeExp, nw: SchemeExp, idn: Identity) extends ChangeExp[SchemeExp] with SchemeExp:
    override def toString: String = s"(<change> $old $nw)"
    override def prettyString(indent: Int): String =
      s"(<change>\n${" " * nextIndent(indent) ++ old.prettyString(nextIndent(indent))}\n${" " * nextIndent(indent) ++ nw.prettyString(nextIndent(indent))})"

trait CSchemeExp extends SchemeExp

/** Fork a thread with an expression to evaluate. */
case class CSchemeFork(body: SchemeExp, idn: Identity) extends CSchemeExp:
    def fv: Set[String] = body.fv
    def label: Label = FRK
    def subexpressions: List[Expression] = List(body)
    override val height: Int = body.height + 1
    override def toString: String = s"(fork $body)"

/** Join a thread, given an expression that should evaluate to a TID. */
case class CSchemeJoin(tExp: SchemeExp, idn: Identity) extends CSchemeExp:
    def fv: Set[String] = tExp.fv
    def label: Label = JOI
    def subexpressions: List[Expression] = List(tExp)
    override val height: Int = tExp.height + 1
    override def toString: String = s"(join $tExp)"

trait ContractSchemeExp extends SchemeExp

case class ContractSchemeDepContract(
    domains: List[SchemeExp],
    rangeMaker: SchemeExp,
    idn: Identity)
    extends ContractSchemeExp:
    def fv: Set[String] = domains.flatMap(_.fv).toSet ++ rangeMaker.fv
    def label: Label = DPC
    def subexpressions: List[Expression] = rangeMaker :: domains
    override def toString: String = s"(~>d ${domains.map(_.toString).mkString(" ")} $rangeMaker)"

case class ContractSchemeFlatContract(
    expression: SchemeExp,
    idn: Identity)
    extends ContractSchemeExp:
    def fv: Set[String] = expression.fv
    def label: Label = FLC
    def subexpressions: List[Expression] = List(expression)
    override def toString: String = s"(flat $expression)"

case class ContractSchemeMon(
    contract: SchemeExp,
    expression: SchemeExp,
    idn: Identity)
    extends ContractSchemeExp:
    def fv: Set[String] = contract.fv ++ expression.fv
    def label: Label = MON
    def subexpressions: List[Expression] = List(contract, expression)
    override def toString: String = s"(mon $contract $expression)"

case class ContractSchemeDefineContract(
    name: Identifier,
    params: List[Identifier],
    contract: SchemeExp,
    expression: SchemeExp,
    idn: Identity)
    extends ContractSchemeExp:
    def fv: Set[String] = contract.fv ++ (expression.fv -- params.map(_.name))
    def label: Label = DFC
    def subexpressions: List[Expression] = List(contract, expression)
    override def toString: String = s"(define/contract ($name ${params.mkString(" ")}) $expression)"

/**
 * (check contract value)
 *
 * Checks a contract on a value. Returns #f if it fails, otherwise #t. Only works on flat contracts.
 */
case class ContractSchemeCheck(
    contract: SchemeExp,
    valueExpression: SchemeExp,
    idn: Identity)
    extends ContractSchemeExp:
    def fv: Set[String] = contract.fv ++ valueExpression.fv
    def label: Label = CHK
    def subexpressions: List[Expression] = List(contract, valueExpression)
    override def toString: String = s"(check $contract $valueExpression)"

/**
 * A racket provide expression.
 *
 * (provide (contract-out (name contract) ...))
 *
 * The form is more expressive in actual Racket programs, but in this analysis it is only included to kickstart the analysis of the contracts.
 *
 * A contract-out expression exposes a function named `name` to external modules and guards it with the given contract.
 */
case class ContractSchemeProvide(
    outs: List[ContractSchemeProvideOut],
    idn: Identity)
    extends ContractSchemeExp:
    def fv: Set[String] = outs.flatMap(_.fv).toSet
    def label: Label = PROV
    def subexpressions: List[Expression] = outs.flatMap(_.subexpressions)

abstract class ContractSchemeProvideOut extends ContractSchemeExp

/** An element of (contract-out ...) */
case class ContractSchemeContractOut(
    name: Identifier,
    contract: SchemeExp,
    idn: Identity)
    extends ContractSchemeProvideOut:
    def fv: Set[String] = contract.fv
    def label: Label = PCO
    def subexpressions: List[Expression] = List(contract)

abstract class MakeStruct extends ContractSchemeExp:
    def fv: Set[String] = Set()
    def subexpressions: List[Expression] = List()

/**
 * Creates a struct getter can be applied like a function
 *
 * (define posn-x (_make_struct_getter 'posn 0)) (posn-x (posn 10 20))
 */
case class MakeStructGetter(
    tag: String,
    idx: Int,
    idn: Identity)
    extends MakeStruct:

    def label = MSG

/**
 * Creates a struct setter can be applied like a function
 *
 * (define set-posn-x! (_make_struct_setter 'posn 0)) (set-posn-x! (posn 10 20) 5)
 */
case class MakeStructSetter(
    tag: String,
    idx: Int,
    idn: Identity)
    extends MakeStruct:
    def label = MSS

/**
 * Creates a constructor, that can be applied like a function.
 *
 * (define posn (_make_struct_constr 'posn 2)) (posn 10 20)
 */
case class MakeStructConstr(
    tag: String,
    siz: Int,
    idn: Identity)
    extends MakeStruct:

    def label = MSC

case class MakeStructPredicate(
    tag: String,
    idn: Identity)
    extends MakeStruct:

    def label = MSP

/**
 * A clause of a match expression.
 *
 * @param pat
 *   the pattern of the clause
 * @param expr
 *   the expression that will be evaluated if the pattern matches
 * @param whenExpr
 *   an optional expression that is evaluated if the pattern matches, and if true will allow expr to be evaluated otherwise the clause is ignored
 */
case class MatchExprClause(pat: MatchPat, expr: List[SchemeExp], whenExpr: Option[SchemeExp]):
    def fv: Set[String] = (expr.flatMap(_.fv).toSet ++ whenExpr.map(_.fv).getOrElse(Set())) -- pat.fv // the free variables in patterns are treated as new variables
    def subexpressions: List[Expression] = pat.subexpressions ++ expr ++ whenExpr.map(List(_)).getOrElse(List())

/**
 * A regular match expression.
 *
 * (match v clause ...)
 *
 * @param value
 *   the expression representing the value that it is matched against
 * @param clauses
 *   a list of match expression clauses
 */
case class MatchExpr(value: SchemeExp, clauses: List[MatchExprClause], idn: Identity) extends SchemeExp:
    def fv: Set[String] = value.fv ++ clauses.flatMap(_.fv)
    def subexpressions: List[Expression] = value :: clauses.flatMap(_.subexpressions)
    def label = MEX
