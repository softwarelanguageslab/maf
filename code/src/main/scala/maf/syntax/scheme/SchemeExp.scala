package maf.syntax.scheme

import maf.syntax.*
import maf.syntax.sexp.*
import maf.util.Show


/** Abstract syntax of Scheme programs */
sealed trait SchemeExp extends Expression:
    def prettyString(indent: Int = 0): String = toString()
    def nextIndent(current: Int): Int = current + 3

object SchemeExp:
    given Show[SchemeExp] with
        def show(v: SchemeExp): String = v.toString
/*
    case SchemeLambda(name, args, body, ann, pos) =>
    case SchemeVarArgLambda(name, args, vararg, body, ann, pos) =>
    case SchemeFuncall(f, args, pos) =>
    case SchemeIf(cond, cons, alt, pos) =>
    case SchemeLet(bindings, body, pos) =>
    case SchemeLetStar(bindings, body, pos) =>
    case SchemeLetrec(bindings, body, pos) =>
    case SchemeSet(variable, value, pos) =>
    case SchemeSetLex(variable, lexAddr, value, pos) =>
    case SchemeBegin(exps, pos) =>
    case SchemeDefineVariable(name, value, pos) =>
    case SchemeDefineFunction(name, args, body, pos) =>
    case SchemeDefineVarArgFunction(name, args, vararg, body, pos) =>
    case SchemeVar(id) =>
    case SchemeVarLex(id, lexAdr) =>
    case SchemeValue(value, pos) =>
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
    // the body as a single expression (a `begin` if the body has several expressions), constructed once so that it is the same program point for every call
    val bodyExp: SchemeExp = SchemeBody(body)
    // does the lambda support a variable number of arguments
    def varArgId: Option[Identifier]
    // can this lambda be called with a given number of arguments
    def check(argc: Int): Boolean =
        if varArgId.isDefined then argc >= args.length
        else argc == args.length
    // a short name for this function
    def lambdaName: String = name.getOrElse(s"λ@${pos.show}")
    // free variables
    lazy val fv: Set[String] = SchemeBody.fv(body) -- args.map(_.name).toSet -- varArgId.map(id => Set(id.name)).getOrElse(Set[String]())

  // height
    override val height: Int = 1 + body.foldLeft(0)((mx, e) => mx.max(e.height))
    def annotation: Option[(String, String)]
    def subexpressions: List[Expression] = args ::: body

case class SchemeLambda(
    name: Option[String],
    args: List[Identifier],
    body: List[SchemeExp],
    annotation: Option[(String, String)],
    pos: Option[Position])
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
    pos: Option[Position])
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
    pos: Option[Position])
    extends SchemeExp:
    override def toString: String =
        if args.isEmpty then s"($f)"
        else
            val a = args.mkString(" ")
            s"($f $a)"
    def fv: Set[String] = f.fv ++ args.flatMap(_.fv).toSet
    override val height: Int = 1 + args.foldLeft(0)((mx, a) => mx.max(a.height).max(f.height))
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
    pos: Option[Position])
    extends SchemeExp:
    override def toString: String = s"(if $cond $cons $alt)"
    def fv: Set[String] = cond.fv ++ cons.fv ++ alt.fv
    override val height: Int = 1 + cond.height.max(cons.height).max(alt.height)
    def subexpressions: List[Expression] = List(cond, cons, alt)

    override def prettyString(indent: Int): String =
        if this.toString.size < 50 then this.toString
        else
            s"(if $cond\n${" " * nextIndent(indent)}${cons.prettyString(nextIndent(indent))}\n${" " * nextIndent(indent)}${alt.prettyString(nextIndent(indent))})"

/** A let-like expression. */
sealed trait SchemeLettishExp extends SchemeExp:
    val bindings: List[(Identifier, SchemeExp)]
    val body: List[SchemeExp]
    val pos: Option[Position]
    override val height: Int = 1 + bindings.foldLeft(0)((mx, b) => mx.max(b._2.height).max(body.foldLeft(0)((mx, e) => mx.max(e.height))))
    def subexpressions: List[Expression] = bindings.foldLeft(List[Expression]())((a, b) => b._2 :: b._1 :: a) ::: body
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
    pos: Option[Position])
    extends SchemeLettishExp:
    override def toString: String =
        val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
        val bo = body.mkString(" ")
        s"(let ($bi) $bo)"
    def fv: Set[String] =
        bindings.map(_._2).flatMap(_.fv).toSet ++ (SchemeBody.fv(body) -- bindings.map(_._1.name).toSet)

    def letName: String = "let"

/** Let*-bindings: (let* ((v1 e1) ...) body...) */
case class SchemeLetStar(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    pos: Option[Position])
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

    def letName: String = "let*"

/** Letrec-bindings: (letrec* ((v1 e1) ...) body...) */
case class SchemeLetrec(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    pos: Option[Position])
    extends SchemeLettishExp:
    override def toString: String =
        val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
        val bo = body.mkString(" ")
        s"(letrec ($bi) $bo)"
    def fv: Set[String] =
        (bindings.map(_._2).flatMap(_.fv).toSet ++ SchemeBody.fv(body)) -- bindings
            .map(_._1.name)
            .toSet
    def letName: String = "letrec"

    if bindings.size > bindings.map(_._1.name).toSet.size then
        throw new Exception(
          s"Illegal letrec: duplicate definitions (${pos.show}): ${bindings.map(_._1.name).groupBy(name => name).view.mapValues(_.size).toList.filter(_._2 > 1).sorted.map(p => s"${p._1} (${p._2})").mkString("{", ", ", "}")}."
        )

/** Named-let: (let name ((v1 e1) ...) body...) */
object SchemeNamedLet:
    def apply(
        name: Identifier,
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        annotation: Option[(String, String)],
        pos: Option[Position]
      ): SchemeExp =
        val (prs, ags) = bindings.unzip
        val fnDef =
            SchemeLetrec(List((name, SchemeLambda(Some(name.name), prs, body, annotation, pos))), List(SchemeVar(Identifier(name.name, pos))), pos)
        SchemeFuncall(fnDef, ags, pos)

/** A set! expression: (set! variable value) */
sealed trait SchemeSetExp extends SchemeExp:
    val variable: Identifier
    val value: SchemeExp
    def fv: Set[String] = value.fv + variable.name
    override val height: Int = 1 + value.height
    def subexpressions: List[Expression] = List(variable, value)
    override def prettyString(indent: Int): String =
        val ind = indent + variable.toString.length + 4
        s"(set! $variable ${value.prettyString(ind)})"

case class SchemeSet(
    variable: Identifier,
    value: SchemeExp,
    pos: Option[Position])
    extends SchemeSetExp:
    override def toString: String = s"(set! $variable $value)"

case class SchemeSetLex(
    variable: Identifier,
    lexAddr: LexicalRef,
    value: SchemeExp,
    pos: Option[Position])
    extends SchemeSetExp:
    override def toString = s"(set! $variable $value)"

/** A begin clause: (begin body...) */
case class SchemeBegin(exps: List[SchemeExp], pos: Option[Position]) extends SchemeExp:
    override def toString: String =
        val body = exps.mkString(" ")
        s"(begin $body)"
    def fv: Set[String] = exps.flatMap(_.fv).toSet
    override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
    def subexpressions: List[Expression] = exps

    override def prettyString(indent: Int): String =
        s"(begin\n${exps.map(" " * nextIndent(indent) ++ _.prettyString(nextIndent(indent))).mkString("\n")})"

/** Used to create a begin if there are multiple statements, and a single exp if there is only one */
object SchemeBody:
    def apply(exps: List[SchemeExp]): SchemeExp = exps match
        case Nil        => SchemeValue(Value.Boolean(false), None) /* undefined */
        case exp :: Nil => exp
        case exp :: _   => SchemeBegin(exps, exp.pos)
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
    def apply(clauses: List[(SchemeExp, List[SchemeExp])], pos: Option[Position]): SchemeExp =
        if clauses.isEmpty then throw new Exception(s"Invalid Scheme cond without clauses (${pos.show})")
        else
            clauses.foldRight[SchemeExp](SchemeValue(Value.Boolean(false /* undefined */ ), None))((clause, acc) =>
                clause match {
                    case (SchemeValue(Value.Boolean(true), _), body) => SchemeBody(body)
                    case (cond, Nil)                                 =>
                        /* Body is empty. R5RS states that "If the selected clause contains only the
                         * test and no expressions ,then the value of the test is returned
                         * as the result" */
                        val id = Identifier("__cond-empty-body", cond.pos)
                        SchemeLet(
                          List((id, cond)),
                          List(SchemeIf(SchemeVar(id), SchemeVar(id), acc, cond.pos)),
                          cond.pos
                        )
                    case (cond, body) => SchemeIf(cond, SchemeBody(body), acc, cond.pos)
                }
            )

/** A when expression: (when pred body ...) Desugared into an if-expression. */
object SchemeWhen:
    def apply(
        pred: SchemeExp,
        body: List[SchemeExp],
        pos: Option[Position]
      ): SchemeExp =
        SchemeIf(pred, SchemeBody(body), SchemeValue(Value.Boolean(false), pos), pos)

/** An unless expression: (unless pred body ...) Desugared into an if-expression. */
object SchemeUnless:
    def apply(
        pred: SchemeExp,
        body: List[SchemeExp],
        pos: Option[Position]
      ): SchemeExp =
        SchemeIf(pred, SchemeValue(Value.Boolean(false), pos), SchemeBody(body), pos)

/** A case expression: (case key ((vals1...) body1...) ... (else default...)). Desugared according to R5RS. */
object SchemeCase:
    def apply(
        key: SchemeExp,
        clauses: List[(List[SchemeValue], List[SchemeExp])],
        default: List[SchemeExp],
        pos: Option[Position]
      ): SchemeExp = key match
        case _: SchemeVar | _: SchemeValue =>
            /** Atomic key */
            val eqv = SchemeVar(Identifier("eq?", None)) /* TODO: should be eqv? instead of eq? */
            clauses.foldRight[SchemeExp](SchemeBody(default))((clause, acc) =>
                /**
                 * In R5RS, the condition is desugared into a (memv key '(atoms ...)) call. This would mean we would have to construct a list and go
                 * through it, which would badly impact precision. Hence, we instead explicitly do a big-or with eq?
                 */
                SchemeIf(
                  SchemeOr(
                    clause._1.map(atom => SchemeFuncall(eqv, List(key, atom), atom.pos)),
                    pos
                  ),
                  SchemeBody(clause._2),
                  acc,
                  pos
                )
            )
        case _ =>
            /** Non-atomic key, let-bind it */
            val id = Identifier("__case-atom-key", key.pos)
            SchemeLet(List((id, key)), List(SchemeCase(SchemeVar(id), clauses, default, pos)), key.pos)

/** An and expression: (and exps...) */
object SchemeAnd:
    def apply(eps: List[SchemeExp], pos: Option[Position]): SchemeExp = eps match
        case Nil        => SchemeValue(Value.Boolean(true), pos)
        case exp :: Nil => exp
        case exp :: rst => SchemeIf(exp, SchemeAnd(rst, pos), SchemeValue(Value.Boolean(false), pos), pos)

/** An or expression: (or exps...) */
object SchemeOr:
    def apply(eps: List[SchemeExp], pos: Option[Position]): SchemeExp = eps match
        case Nil        => SchemeValue(Value.Boolean(false), pos)
        case exp :: Nil => exp
        case exp :: rst =>
            val tmp = "__or_res"
            SchemeLet(
              List((Identifier(tmp, exp.pos), exp)),
              List((SchemeIf(SchemeVar(Identifier(tmp, None)), SchemeVar(Identifier(tmp, None)), SchemeOr(rst, pos), pos))),
              pos
            )

/** A variable definition: (define name value) */
case class SchemeDefineVariable(
    name: Identifier,
    value: SchemeExp,
    pos: Option[Position])
    extends SchemeExp:
    override def toString: String = s"(define $name $value)"
    def fv: Set[String] = value.fv - name.name
    override val height: Int = 1 + value.height
    def subexpressions: List[Expression] = List(name, value)

    override def prettyString(indent: Int): String = s"(define $name ${value.prettyString(nextIndent(indent))})"

/** Function definition of the form (define (f arg ...) body) */
object SchemeDefineFunction:
    def apply(name: Identifier, args: List[Identifier], body: List[SchemeExp], annotation: Option[(String, String)], pos: Option[Position]): SchemeExp =
        SchemeDefineVariable(name, SchemeLambda(Some(name.name), args, body, annotation, pos), pos)

/** Function definition with varargs of the form (define (f arg . vararg ...) body) */
object SchemeDefineVarArgFunction:
    def apply(
        name: Identifier,
        args: List[Identifier],
        vararg: Identifier,
        body: List[SchemeExp],
        annotation: Option[(String, String)],
        pos: Option[Position]
      ): SchemeExp =
        SchemeDefineVariable(name, SchemeVarArgLambda(Some(name.name), args, vararg, body, annotation, pos), pos)

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
        pos: Option[Position]
      ): SchemeExp =
        val loopIdName = "__do_loop"
        val loopId = Identifier(loopIdName, pos)
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
                                          pos
                            )
                          )
                    ),
                    pos
                  )
                ),
                annot,
                pos
              )
            )
          ),
          List(SchemeFuncall(SchemeVar(loopId), vars.map(_._2), pos)),
          pos
        )

/** An identifier: name */
sealed trait SchemeVarExp extends SchemeExp:
    val id: Identifier
    override val height: Int = 1
    val pos: Option[Position] = id.pos
    def fv: Set[String] = Set(id.name)
    def subexpressions: List[Expression] = List(id)

case class SchemeVar(id: Identifier) extends SchemeVarExp:
    override def toString: String = id.name

case class SchemeVarLex(id: Identifier, lexAddr: LexicalRef) extends SchemeVarExp:
    override def toString: String = id.name

object SchemePair:
    def apply(car: SchemeExp, cdr: SchemeExp, pos: Option[Position]): SchemeExp =
        SchemeFuncall(SchemeVar(Identifier("__toplevel_cons", pos)), List(car, cdr), pos)

object SchemeSplicedPair:
    def apply(splice: SchemeExp, cdr: SchemeExp, pos: Option[Position]): SchemeExp =
        SchemeFuncall(SchemeVar(Identifier("__toplevel_append", pos)), List(splice, cdr), pos)

/** A   value (number, symbol, string, ...) */
case class SchemeValue(value: Value, pos: Option[Position]) extends SchemeExp:
    override def toString: String = value.toString
    def fv: Set[String] = Set()
    override val height: Int = 1
    def subexpressions: List[Expression] = List()

/** An assertion (assert <exp>) */
case class SchemeAssert(exp: SchemeExp, pos: Option[Position]) extends SchemeExp:
    override def toString: String = s"(assert $exp)"
    def fv: Set[String] = exp.fv
    def subexpressions: List[Expression] = List(exp)

/** Creates explicit (mutable) reference */
object SchemeRef:
    def apply(exp: SchemeExp, pos: Option[Position]) =
        SchemeFuncall(SchemeVar(Identifier("__toplevel_cons", pos)), List(SchemeValue(Value.Symbol("ref"), pos), exp), pos)

/** Dereferences an explicit (mutable) reference */
object SchemeDeref:
    def apply(ref: SchemeExp, pos: Option[Position]) =
        SchemeFuncall(SchemeVar(Identifier("__toplevel_cdr", pos)), List(ref), pos)

/** Updates an explicit (mutable) reference */
object SchemeSetRef:
    def apply(ref: SchemeExp, exp: SchemeExp, pos: Option[Position]) =
        SchemeFuncall(SchemeVar(Identifier("__toplevel_set-cdr!", pos)), List(ref, exp), pos)
