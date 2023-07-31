package maf.language.scheme

import maf.core.*
import maf.language.change.ChangeExp
import maf.language.sexp.*
import maf.language.scheme.ContractSchemeExp
import maf.language.ContractScheme.MatchPat
import Label.*
import maf.deltaDebugging.primitiveOpNames.PrimitiveOpNames
import maf.language.scheme
import maf.language.scheme.primitives.SchemePrelude
import maf.util.Show
import maf.language.racket.{Modules, ProvideDirective, RequireDirective, ResolvedRequire, SelectedProvide}

/** Abstract syntax of Scheme programs */
sealed trait SchemeExp extends Expression:
    type T <: SchemeExp
    def levelNodes(level: Int): List[SchemeExp] =
      if level == 0 then
        List(this)
      else
        this.subexpressions.collect { case s: SchemeExp => s }.flatMap(s => s. levelNodes (level - 1))
    def deepDropIdentifier(id: Identifier): Option[SchemeExp] = None
    /** deleteChildren */
    def deleteChildren(fnc: SchemeExp => Boolean): Option[T] = ???
    /** Replace */
    def replace(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      require(this.contains(node)) //a safety check, ensuring replacement can occur
      replaceThis(node, replacement)
    def replaceThis(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      if this eq node then
        replacement
      else replaceLower(node, replacement)
    def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp = ???
    /** map & forEach */
    def map(f: SchemeExp => SchemeExp): SchemeExp = f(this).mapLower(f)
    def mapLower(f: SchemeExp => SchemeExp): SchemeExp = ???
    def forEach(f: SchemeExp => Unit): Unit =
      this.map(e => {
        f(e)
        e
      })
    /** contains */
    def contains(exp: SchemeExp): Boolean =
      (this eq exp) || this.allSubexpressions.exists(subExp => subExp eq exp)
    /** usedSet */
    def usedSet(): List[Identifier] =
      var usedSet: List[Identifier] = List()

      def add(i: Identifier): Unit =
        if !usedSet.exists(id => id.name equals i.name) then
          usedSet = usedSet.::(i)

      this.forEach(e => {
        e match
          case exp: SchemeVarExp =>
            if !(PrimitiveOpNames.allNames contains exp.id.name) then
              add(exp.id)
          case _ =>
      })

      usedSet
    /** definedSet */
    def definedSet(): List[Identifier] =
      var definedSet: List[Identifier] = List()

      def add(i: Identifier): Unit =
        if !definedSet.exists(id => id.name equals i.name) then
          definedSet = definedSet.::(i)

      this.forEach(e => {
        e match
          case exp: SchemeVarArgLambda =>
            exp.args.foreach(identifier => add(identifier))
            add(exp.vararg)
          case exp: SchemeLambdaExp =>
            exp.args.foreach(identifier => add(identifier))
          case exp: SchemeLettishExp =>
            exp.bindings.map(_._1).foreach(identifier => add(identifier))
          case SchemeDefineVariable(name, value, idn) =>
            add(name)
          case _ =>
      })

      definedSet
    /** findUndefinedVariables */
    def findUndefinedVariables(): List[Identifier] = {
      val usedSet: List[Identifier] = this.usedSet()
      val definedSet: List[Identifier] = this.definedSet()

      usedSet.filterNot(usedId => definedSet.exists(definedId => definedId.name equals usedId.name))
    }

    def sexpCopy(): T = ???
    def prettyString(indent: Int = 0): String = toString()
    def nextIndent(current: Int): Int = current + 3
    def parent(someDescendant: SchemeExp): Option[SchemeExp] =
      val subSchemeExps = subexpressions.collect({
        case s: SchemeExp => s
      })
      if subSchemeExps.isEmpty then
        None
      else if subSchemeExps.exists(subExp => subExp eq someDescendant) then
        Some(this)
      else subSchemeExps.find(subExp => {
        subExp.parent(someDescendant).nonEmpty
      })

object SchemeExp:
    given Show[SchemeExp] with
        def show(v: SchemeExp): String = v.toString
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
    def shallowDropIdentifier(id: Identifier): Option[SchemeExp]

case class SchemeLambda(
    name: Option[String],
    args: List[Identifier],
    body: List[SchemeExp],
    annotation: Option[(String, String)],
    idn: Identity)
    extends SchemeLambdaExp:
    override type T = SchemeLambda

    override def toString: String =
        val a = args.mkString(" ")
        val b = body.mkString(" ")
        s"(lambda ($a) $b)"
    def varArgId: Option[Identifier] = None

    override def sexpCopy(): T =
      SchemeLambda(name, args, body.map(_.sexpCopy()), annotation, idn)

    override def deepDropIdentifier(id: Identifier): Option[T] =
      val remainingBody = body.map(_.deepDropIdentifier(id)).collect({
        case Some(exp) => exp
      })
      if remainingBody.nonEmpty then
        Some(SchemeLambda(name, args.filterNot(_.name equals id.name), remainingBody, annotation, idn))
      else None

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      val newBody: List[SchemeExp] = body.filterNot(fnc).map(_.deleteChildren(fnc)).collect({
        case Some(e) => e
      })
      if newBody.isEmpty then
        None
      else Some(SchemeLambda(name, args, newBody, annotation, idn))

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeLambda(name, args, body.map(_.replaceThis(node, replacement)), annotation, idn)
    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeLambda(name, args, body.map(sexp => sexp.map(f)), annotation, idn)
    override def prettyString(indent: Int): String =
        s"(lambda (${args.mkString(" ")})\n${body.map(" " * nextIndent(indent) ++ _.prettyString(nextIndent(indent))).mkString("\n")})"

    override def shallowDropIdentifier(id: Identifier): Option[SchemeExp] =
      val paramDropped = SchemeLambda(name, args.filterNot(a => a.name equals id.name), body, annotation, idn)

      val dropped: Option[SchemeExp] = paramDropped.deleteChildren(lambdaChild => {
        lambdaChild.allSubexpressions.exists(exp => {
          exp match
            case subId: Identifier =>
              subId.name equals id.name
            case _ => false
        })
      })

      dropped match
        case Some(lambdaExp) =>
          val definedSet = this.definedSet()
          val definedSetAfterDrop = lambdaExp.definedSet()
          val lostBindings = definedSet.filterNot(preDefinedId => definedSetAfterDrop.exists(postDefinedId => postDefinedId.name equals preDefinedId.name))

          lostBindings.foldRight(dropped)((lostBinding, maybeLet) => maybeLet match
            case Some(lambda: SchemeLambdaExp) => lambda.shallowDropIdentifier(lostBinding)
            case _ => None
          )
        case _ => None


case class SchemeVarArgLambda(
    name: Option[String],
    args: List[Identifier],
    vararg: Identifier,
    body: List[SchemeExp],
    annotation: Option[(String, String)],
    idn: Identity)
    extends SchemeLambdaExp:
    override type T = SchemeVarArgLambda

    override def toString: String =
        val a =
            if args.isEmpty then vararg.toString
            else s"(${args.mkString(" ")} . $vararg)"
        val b = body.mkString(" ")
        s"(lambda $a $b)"
    def varArgId: Option[Identifier] = Some(vararg)

    override def sexpCopy(): T =
      SchemeVarArgLambda(name, args, vararg, body.map(_.sexpCopy()), annotation, idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      val remainingBody = body.map(_.deepDropIdentifier(id)).collect({
        case Some(exp) => exp
      })
      if remainingBody.nonEmpty then
        Some(SchemeVarArgLambda(name, args, vararg, remainingBody, annotation, idn))
      else None

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      val newBody: List[SchemeExp] = body.filterNot(fnc).map(_.deleteChildren(fnc)).collect({
        case Some(e) => e
      })
      if newBody.isEmpty then
        None
      else Some(SchemeVarArgLambda(name, args, vararg, newBody, annotation, idn))

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeVarArgLambda(name, args, vararg, body.map(_.replaceThis(node, replacement)), annotation, idn)
    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeVarArgLambda(name, args, vararg, body.map(sexp => sexp.map(f)), annotation, idn)
    override def prettyString(indent: Int): String =
        val a =
            if args.isEmpty then vararg.toString
            else s"(${args.mkString(" ")} . $vararg)"
        s"(lambda $a\n${body.map(" " * nextIndent(indent) ++ _.prettyString(nextIndent(indent))).mkString("\n")})"

    override def shallowDropIdentifier(id: Identifier): Option[SchemeExp] = None

/** A function call: (f args...) */
case class SchemeFuncall(
    f: SchemeExp,
    args: List[SchemeExp],
    idn: Identity)
    extends SchemeExp:
    override type T = SchemeFuncall
    override def toString: String =
        if args.isEmpty then s"($f)"
        else
            val a = args.mkString(" ")
            s"($f $a)"
    def fv: Set[String] = f.fv ++ args.flatMap(_.fv).toSet
    override val height: Int = 1 + args.foldLeft(0)((mx, a) => mx.max(a.height).max(f.height))
    val label: Label = FNC
    def subexpressions: List[Expression] = f :: args
    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      val newSubExps: List[SchemeExp] = (List(f) ++ args).filterNot(fnc).map(_.deleteChildren(fnc)).collect({
        case Some(e) => e
      })
      if newSubExps.isEmpty then
        None
      else Some(SchemeFuncall(newSubExps.head, newSubExps.tail, idn))

    override def sexpCopy(): T =
      SchemeFuncall(f.sexpCopy(), args.map(_.sexpCopy()), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      val remainingSubexps = (List(f) ++ args).map(_.deepDropIdentifier(id)).collect({
        case Some(exp) => exp
      })
      if remainingSubexps.isEmpty then
        None
      else Some(SchemeFuncall(remainingSubexps.head, remainingSubexps.tail, idn))

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeFuncall(f.replaceThis(node, replacement), args.map(a => a.replaceThis(node, replacement)), idn)

    override def mapLower(fnc: SchemeExp => SchemeExp): SchemeExp =
      SchemeFuncall(f.map(fnc), args.map(a => a.map(fnc)), idn)

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
    override type T = SchemeIf
    override def toString: String = s"(if $cond $cons $alt)"
    def fv: Set[String] = cond.fv ++ cons.fv ++ alt.fv
    override val height: Int = 1 + cond.height.max(cons.height).max(alt.height)
    val label: Label = IFF
    def subexpressions: List[Expression] = List(cond, cons, alt)

    override def sexpCopy(): T =
      SchemeIf(cond.sexpCopy(), cons.sexpCopy(), alt.sexpCopy(), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      val remainingSubexps = List(cond, cons, alt).map(_.deepDropIdentifier(id)).collect({
        case Some(exp) => exp
      })
      if remainingSubexps.length == 3 then
        Some(SchemeIf(remainingSubexps.head, remainingSubexps(1), remainingSubexps(2), idn))
      else
        Some(SchemeBegin(
          remainingSubexps,
          NoCodeIdentity
        ))

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      val newSubExps = List(cond, cons, alt).filterNot(fnc).map(_.deleteChildren(fnc)).collect({
        case Some(e) => e
      })
      if newSubExps.length == 3 then
        Some(SchemeIf(newSubExps(0), newSubExps(1), newSubExps(2), idn))
      else None

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeIf(cond.replaceThis(node, replacement),
                    cons.replaceThis(node, replacement),
                    alt.replaceThis(node, replacement),
                    idn)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeIf(cond.map(f), cons.map(f), alt.map(f), idn)

    override def prettyString(indent: Int): String =
        if this.toString.size < 50 then this.toString
        else
            s"(if $cond\n${" " * nextIndent(indent)}${cons.prettyString(nextIndent(indent))}\n${" " * nextIndent(indent)}${alt.prettyString(nextIndent(indent))})"

/** A let-like expression. */
sealed trait SchemeLettishExp extends SchemeExp:
    val bindings: List[(Identifier, SchemeExp)]
    val body: List[SchemeExp]
    val idn: Identity
    override type T <: SchemeLettishExp
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

    def deleteChildren(fnc: SchemeExp => Boolean,
                       factoryMethod: (List[(Identifier, SchemeExp)], List[SchemeExp], Identity) => T): Option[T] =
      val newBindings: List[(Identifier, SchemeExp)] =
        bindings.filterNot(binding => fnc(binding._2))
          .map(binding => (binding._1, binding._2.deleteChildren(fnc))).collect({
          case (i: Identifier, Some(e)) => (i, e)
        })
      val newBody: List[SchemeExp] = body.filterNot(fnc).map(_.deleteChildren(fnc)).collect({
        case Some(e) => e
      })
      if newBody.nonEmpty then
        Some(factoryMethod(
          newBindings,
          newBody,
          idn
        ))
      else None

    def dropBinding(bindingName: String): T
    def dropBodyExp(i: Int): T

    def shallowDropIdentifier(id: Identifier): Option[SchemeExp] = None

    def shallowDropIdentifier(id: Identifier,
                              factoryMethod: (List[(Identifier, SchemeExp)], List[SchemeExp], Identity) => SchemeLettishExp): Option[SchemeExp] =
      val bindingDropped = factoryMethod(bindings.filterNot(b => b._1.name equals id.name), body, idn)
      val shallowDropped: Option[SchemeLettishExp] = bindingDropped.deleteChildren(letChild => {
        letChild.allSubexpressions.exists(exp => {
          exp match
            case subId: Identifier =>
              subId.name equals id.name
            case _ => false
        })
      })

      shallowDropped match
        case Some(letExp) =>
          val definedSet = this.definedSet()
          val definedSetAfterDrop = letExp.definedSet()
          val lostBindings = definedSet.filterNot(preDefinedId => definedSetAfterDrop.exists(postDefinedId => postDefinedId.name equals preDefinedId.name))

          lostBindings.foldRight(shallowDropped: Option[SchemeExp])((lostBinding, maybeLet) => maybeLet match
            case Some(let: SchemeLettishExp) => let.shallowDropIdentifier(lostBinding, factoryMethod)
            case _ => None
          )
        case _ => None


/** Let-bindings: (let ((v1 e1) ...) body...) */
case class SchemeLet(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp:
    override type T = SchemeLet
    override def toString: String =
        val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
        val bo = body.mkString(" ")
        s"(let ($bi) $bo)"
    def fv: Set[String] =
        bindings.map(_._2).flatMap(_.fv).toSet ++ (SchemeBody.fv(body) -- bindings.map(_._1.name).toSet)

    override def sexpCopy(): T =
      SchemeLet(bindings.map(b => (b._1, b._2.sexpCopy())), body.map(_.sexpCopy()), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      val deepDroppedBindings = bindings.filterNot(b => b._1.name equals id.name).map(b => (b._1, b._2.deepDropIdentifier(id)))
      val remainingBindings = deepDroppedBindings.collect({ case (identifier, Some(exp)) => (identifier, exp)})
      val lostBindings = deepDroppedBindings.collect({case (identifier, None) => identifier})

      val remainingBody = body.map(_.deepDropIdentifier(id)).collect({
        case Some(exp) => exp
      })
      if remainingBody.isEmpty then
        None
      else if lostBindings.isEmpty then
        Some(SchemeLet(remainingBindings, remainingBody, idn))
      else lostBindings.foldRight(Some(SchemeLet(remainingBindings, remainingBody, idn)): Option[SchemeExp])((lostBinding, maybeLet) => maybeLet match
        case Some(let) => let.deepDropIdentifier(lostBinding)
        case _ => None
      )

    override def dropBinding(name: String): SchemeLet =
      SchemeLet(bindings.filterNot(tpl => tpl._1.name equals name), body, idn)

    override def dropBodyExp(i: Int): SchemeLet =
      SchemeLet(bindings, body.take(i) ++ body.drop(i + 1), idn)

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      deleteChildren(fnc, SchemeLet.apply)

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeLet(bindings.map(b => (b._1, b._2.replaceThis(node, replacement))), body.map(exp => exp.replaceThis(node, replacement)), idn)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeLet(bindings.map(b => (b._1, b._2.map(f))), body.map(sexp => sexp.map(f)), idn)

    override def shallowDropIdentifier(id: Identifier): Option[SchemeExp] =
      super.shallowDropIdentifier(id, SchemeLet.apply)

    val label: Label = LET
    def letName: String = "let"

/** Let*-bindings: (let* ((v1 e1) ...) body...) */
case class SchemeLetStar(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp:
    override type T = SchemeLetStar

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

    override def sexpCopy(): T =
      SchemeLetStar(bindings.map(b => (b._1, b._2.sexpCopy())), body.map(_.sexpCopy()), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      val deepDroppedBindings = bindings.filterNot(b => b._1.name equals id.name).map(b => (b._1, b._2.deepDropIdentifier(id)))
      val remainingBindings = deepDroppedBindings.collect({ case (identifier, Some(exp)) => (identifier, exp) })
      val lostBindings = deepDroppedBindings.collect({ case (identifier, None) => identifier })

      val remainingBody = body.map(_.deepDropIdentifier(id)).collect({
        case Some(exp) => exp
      })
      if remainingBody.isEmpty then
        None
      else if lostBindings.isEmpty then
        Some(SchemeLetStar(remainingBindings, remainingBody, idn))
      else lostBindings.foldRight(Some(SchemeLetStar(remainingBindings, remainingBody, idn)): Option[SchemeExp])((lostBinding, maybeLet) => maybeLet match
        case Some(let) => let.deepDropIdentifier(lostBinding)
        case _ => None
      )

    override def dropBinding(name: String): SchemeLetStar =
      SchemeLetStar(bindings.filterNot(tpl => tpl._1.name equals name), body, idn)

    override def dropBodyExp(i: Int): SchemeLetStar =
      SchemeLetStar(bindings, body.take(i) ++ body.drop(i + 1), idn)

    override def shallowDropIdentifier(id: Identifier): Option[SchemeExp] =
      super.shallowDropIdentifier(id, SchemeLetStar.apply)

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      deleteChildren(fnc, SchemeLetStar.apply)

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeLetStar(bindings.map(b => (b._1 , b._2.replaceThis(node, replacement))), body.map(exp => exp.replaceThis(node, replacement)), idn)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeLetStar(bindings.map(b => (b._1, b._2.map(f))), body.map(sexp => sexp.map(f)), idn)

    val label: Label = LTS
    def letName: String = "let*"

/** Letrec-bindings: (letrec* ((v1 e1) ...) body...) */
case class SchemeLetrec(
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    idn: Identity)
    extends SchemeLettishExp:
    override type T = SchemeLetrec
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

    override def sexpCopy(): T =
      SchemeLetrec(bindings.map(b => (b._1, b._2.sexpCopy())), body.map(_.sexpCopy()), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      val deepDroppedBindings = bindings.filterNot(b => b._1.name equals id.name).map(b => (b._1, b._2.deepDropIdentifier(id)))
      val remainingBindings = deepDroppedBindings.collect({ case (identifier, Some(exp)) => (identifier, exp) })
      val lostBindings = deepDroppedBindings.collect({ case (identifier, None) => identifier })

      val remainingBody = body.map(_.deepDropIdentifier(id)).collect({
        case Some(exp) => exp
      })
      if remainingBody.isEmpty then
        None
      else if lostBindings.isEmpty then
        Some(SchemeLetrec(remainingBindings, remainingBody, idn))
      else lostBindings.foldRight(Some(SchemeLetrec(remainingBindings, remainingBody, idn)): Option[SchemeExp])((lostBinding, maybeLet) => maybeLet match
        case Some(let) => let.deepDropIdentifier(lostBinding)
        case _ => None
      )

    override def dropBinding(name: String): SchemeLetrec =
      SchemeLetrec(bindings.filterNot(tpl => tpl._1.name equals name), body, idn)

    override def dropBodyExp(i: Int): SchemeLetrec =
      SchemeLetrec(bindings, body.take(i) ++ body.drop(i + 1), idn)

    override def shallowDropIdentifier(id: Identifier): Option[SchemeExp] =
      super.shallowDropIdentifier(id, SchemeLetrec.apply)

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      deleteChildren(fnc, SchemeLetrec.apply)

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeLetrec(bindings.map(b => (b._1, b._2.replaceThis(node, replacement))), body.map(exp => exp.replaceThis(node, replacement)), idn)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeLetrec(bindings.map(b => (b._1, b._2.map(f))), body.map(sexp => sexp.map(f)), idn)

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
    override type T = SchemeSet
    override def toString: String = s"(set! $variable $value)"

    override def sexpCopy(): T =
      SchemeSet(variable, value.sexpCopy(), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      value.deepDropIdentifier(id) match
        case Some(exp) =>
          if variable eql id then
            Some(exp)
          else Some(copy(value = exp))
        case None => None

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      value.deleteChildren(fnc) match
        case Some(e) => Some(SchemeSet(variable, e, idn))
        case None => None

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeSet(variable, value.replaceThis(node, replacement), idn)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeSet(variable, value.map(f), idn)

case class SchemeSetLex(
    variable: Identifier,
    lexAddr: LexicalRef,
    value: SchemeExp,
    idn: Identity)
    extends SchemeSetExp:
    override type T = SchemeSetLex
    override def sexpCopy(): T =
      SchemeSetLex(variable, lexAddr, value.sexpCopy(), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      value.deepDropIdentifier(id) match
        case Some(exp) =>
          if variable eql id then
            Some(exp)
          else Some(copy(value = exp))
        case None => None

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      value.deleteChildren(fnc) match
        case Some(e) => Some(SchemeSetLex(variable, lexAddr, e, idn))
        case None => None

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeSetLex(variable, lexAddr, value.replaceThis(node, replacement), idn)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeSetLex(variable, lexAddr, value.map(f), idn)

    override def toString = s"(set! $variable $value)"

/** A begin clause: (begin body...) */
case class SchemeBegin(exps: List[SchemeExp], idn: Identity) extends SchemeExp:
    override type T = SchemeBegin
    override def toString: String =
        val body = exps.mkString(" ")
        s"(begin $body)"
    def fv: Set[String] = exps.flatMap(_.fv).toSet
    override val height: Int = 1 + exps.foldLeft(0)((mx, e) => mx.max(e.height))
    val label: Label = BEG
    def subexpressions: List[Expression] = exps

    override def sexpCopy(): T =
      SchemeBegin(exps.map(_.sexpCopy()), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      val deletedExps = exps.map(_.deepDropIdentifier(id)).collect({
        case Some(exp) => exp
      })
      if deletedExps.isEmpty then
        None
      else Some(copy(exps = deletedExps))

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      val newBody = exps.filterNot(fnc).map(_.deleteChildren(fnc)).collect({
        case Some(e) => e
      })

      if newBody.isEmpty then
        None
      else Some(SchemeBegin(newBody, idn))

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeBegin(exps.map(e => e.replaceThis(node, replacement)), idn)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeBegin(exps.map(e => e.map(f)), idn)

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
    override type T = SchemeDefineVariable
    override def toString: String = s"(define $name $value)"
    def fv: Set[String] = value.fv - name.name
    override val height: Int = 1 + value.height
    val label: Label = DFV
    def subexpressions: List[Expression] = List(name, value)

    override def sexpCopy(): T =
      SchemeDefineVariable(name, value.sexpCopy(), idn)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      if name eql id then
        None
      else
        value.deepDropIdentifier(id) match
          case Some(exp) => Some(copy(value = exp))
          case None => None

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      value.deleteChildren(fnc) match
        case Some(e) => Some(SchemeDefineVariable(name, e, idn))
        case None => None

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeDefineVariable(name, value.replaceThis(node, replacement), idn)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeDefineVariable(name, value.map(f), idn)

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
    override type T = SchemeVar
    override def sexpCopy(): T =
      SchemeVar(id)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      if this.id eql id then
        None
      else Some(copy())

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      Some(SchemeVar(id))

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeVar(id)

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeVar(id)

    override def toString: String = id.name

case class SchemeVarLex(id: Identifier, lexAddr: LexicalRef) extends SchemeVarExp:
    override type T = SchemeVarLex
    override def sexpCopy(): T =
      SchemeVarLex(id, lexAddr)

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      if this.id eql id then
        None
      else Some(copy())

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      Some(SchemeVarLex(id, lexAddr))
    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeVarLex(id, lexAddr)
    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeVarLex(id, lexAddr)
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
    override type T = SchemeValue
    override def sexpCopy(): T =
      SchemeValue(value, idn)
    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      Some(copy())
    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      Some(SchemeValue(value, idn))
    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeValue(value, idn)
    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeValue(value, idn)
    override lazy val hash: Int = (label, value).hashCode()

/** An assertion (assert <exp>) */
case class SchemeAssert(exp: SchemeExp, idn: Identity) extends SchemeExp:
    override type T = SchemeAssert
    override def toString: String = s"(assert $exp)"
    def fv: Set[String] = exp.fv
    val label: Label = ASS

    override def deepDropIdentifier(id: Identifier): Option[SchemeExp] =
      exp.deepDropIdentifier(id) match
        case Some(exp) => Some(copy(exp = exp))
        case None => None

    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeAssert(exp.map(f), idn)

    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeAssert(exp.replaceThis(node, replacement), idn)

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
      exp.deleteChildren(fnc) match
        case Some(e) => Some(SchemeAssert(e, idn))
        case None => None
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
    override type T = SchemeCodeChange
    override def toString: String = s"(<change> $old $nw)"
    override def mapLower(f: SchemeExp => SchemeExp): SchemeExp =
      SchemeCodeChange(old.map(f), nw.map(f), idn)
    override def replaceLower(node: SchemeExp, replacement: SchemeExp): SchemeExp =
      SchemeCodeChange(old.replaceThis(node, replacement), nw.replaceThis(node, replacement), idn)
    override def prettyString(indent: Int): String =
        s"(<change>\n${" " * nextIndent(indent) ++ old.prettyString(nextIndent(indent))}\n${" " * nextIndent(indent) ++ nw.prettyString(nextIndent(indent))})"

    override def deleteChildren(fnc: SchemeExp => Boolean): Option[T] =
        (old.deleteChildren(fnc), nw.deleteChildren(fnc)) match {
            case (Some(o: SchemeExp), Some(n: SchemeExp)) => Some(SchemeCodeChange(o, n, idn))
            case _ => None
        }

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

//
// Actor Language
//

trait ASchemeExp extends SchemeExp

/** A synthetic Scheme expression that only has an identity and no subexpressions */
case class ASchemeSynthetic(idn: Identity) extends ASchemeExp:
    def fv: Set[String] = Set()
    def label: Label = SYN
    def subexpressions: List[Expression] = List()
    override def toString: String = s"<SYN at $idn>"

case class ASchemeActor(
    parameters: List[Identifier],
    selection: SchemeExp,
    idn: Identity,
    name: Option[String],
    isMirror: Boolean)
    extends ASchemeExp:
    def fv: Set[String] =
        selection.fv -- parameters.map(_.name)
    def label: Label = ACT
    def subexpressions: List[Expression] = selection.subexpressions
    override def toString: String =
        val pars = parameters.mkString(" ")
        s"(actor ($pars) $selection)"

case class ASchemeSelect(
    handlers: Map[String, (List[Identifier], List[SchemeExp])],
    idn: Identity)
    extends ASchemeExp:
    def fv: Set[String] =
        handlers.values.flatMap(_._2.flatMap(_.fv)).toSet
    def label: Label = SEL
    def subexpressions: List[Expression] = handlers.values.flatMap(_._2).toList
    override def toString: String =
        val handlersStr = handlers.map { case (nam, (prs, bdy)) => s"($nam (${prs.mkString(" ")}) ${bdy.mkString(" ")})" }.mkString("\n")
        s"$handlersStr"

case class ASchemeSend(actorRef: SchemeExp, messageType: Identifier, arguments: List[SchemeExp], idn: Identity) extends ASchemeExp:
    def fv: Set[String] = actorRef.fv ++ arguments.flatMap(_.fv)
    def label: Label = SEN
    def subexpressions: List[Expression] = actorRef :: arguments
    override def toString: String = s"(send $actorRef $messageType ${arguments.mkString(" ")})"

case class ASchemeBecome(behavior: SchemeExp, arguments: List[SchemeExp], idn: Identity) extends ASchemeExp:
    def fv: Set[String] = behavior.fv ++ arguments.flatMap(_.fv)
    def label: Label = BEC
    def subexpressions: List[Expression] = behavior :: arguments
    override def toString: String = s"(become $behavior ${arguments.mkString(" ")})"

case class ASchemeCreate(behavior: SchemeExp, arguments: List[SchemeExp], idn: Identity) extends ASchemeExp:
    def fv: Set[String] = behavior.fv ++ arguments.flatMap(_.fv)
    def label: Label = CREA
    def subexpressions: List[Expression] = behavior :: arguments
    override def toString: String = s"(create $behavior ${arguments.mkString(" ")})"

// (ask actor msg args ...)
case class ASchemeAsk(actorRef: SchemeExp, messageType: Identifier, args: List[SchemeExp], idn: Identity) extends ASchemeExp:
    def fv: Set[String] = actorRef.fv ++ args.flatMap(_.fv)
    def label: Label = ASK
    def subexpressions: List[Expression] = actorRef :: args
    override def toString: String = s"(ask $actorRef $messageType ${args.mkString(" ")})"

// (await future)
case class ASchemeAwait(future: SchemeExp, idn: Identity) extends ASchemeExp:
    def fv: Set[String] = future.fv
    def label: Label = AWAIT
    def subexpressions: List[Expression] = List(future)
    override def toString: String = s"(await $future)"

//
// Contract Language
//

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

///////////////////////////////////////////////////////
// Contracts for actors
///////////////////////////////////////////////////////

/* Syntax for message/c based contracts */
case class AContractSchemeMessage(tag: String, argumentContracts: List[SchemeExp], ensureContract: SchemeExp, idn: Identity) extends SchemeExp:
    def fv: Set[String] = (argumentContracts.flatMap(_.fv) ++ ensureContract.fv).toSet
    def label: Label = MCC
    def subexpressions: List[Expression] = ensureContract :: argumentContracts

///////////////////////////////////////////////////////
// Racket Utilities
///////////////////////////////////////////////////////

/**
 * A Racket module, as a synthetic AST node.
 *
 * The RacketLoader generates a Racket module whenever it encounters a `#lang racket` at the beginning of the filter
 *
 * @param imports
 *   a map build by the compiler consisting of identifiers mapped to the modules that there are coming from
 */
case class RacketModule(
    name: Modules.Name,
    requiresDirectives: List[RequireDirective],
    providesDirectives: List[ProvideDirective],
    includes: List[ResolvedRequire],
    provides: List[SelectedProvide],
    bdy: SchemeExp,
    idn: Identity)
    extends SchemeExp:
    // The required module identifiers should be visible (and therefore free) within the module,
    // all other variables should be imported from other modules or defined locally.
    override def fv: Set[String] = Set() // TODO: requiresDirectives.map(_.moduleName).toSet
    override def label: Label = MOD
    override def subexpressions: List[Expression] = List(bdy)
    override def toString: String = s"(module $bdy)"

case class RacketRequire(clauses: List[SchemeExp], idn: Identity) extends SchemeExp:
    override def fv: Set[String] = clauses.flatMap(_.fv).toSet
    def label: Label = REQ
    override def subexpressions: List[Expression] = List()
    override def toString: String = s"(require ${clauses.map(_.toString).mkString(" ")})"

case class RacketProvide(clauses: List[SchemeExp], idn: Identity) extends SchemeExp:
    override def fv: Set[String] = clauses.flatMap(_.fv).toSet
    def label: Label = PROV
    override def subexpressions: List[Expression] = List()
    override def toString: String = s"(provide ${clauses.map(_.toString).mkString(" ")})"

// (module-load module-exp ident)
// Lookup the given identifier in the given module
// This is a synthetic syntax element.
case class RacketModuleLoad(module: SchemeExp, name: Identifier, idn: Identity) extends SchemeExp:
    override def fv: Set[String] = module.fv
    def label: Label = RMOD
    override def subexpressions: List[Expression] = List(module)
    override def toString(): String = s"(module-load $module $name)"

// (module-expose (original-name exposed-name) ...)
case class RacketModuleExpose(exposed: Map[String, String], idn: Identity) extends SchemeExp:
    override def fv: Set[String] = Set()
    def label: Label = REXP
    override def subexpressions: List[Expression] = List()
    override def toString: String = s"(expose ${exposed.map { case (k, v) => s"($k $v)" }.mkString(" ")})"

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

/**
 * A "hole" that is used in symbolic expression to absorb symbolic expressions corresponding to that hole
 *
 * @param s
 *   the expression that must be a subexpression of this hole
 */
case class SymbolicHole(s: SchemeExp) extends SchemeExp:
    def idn: Identity = Identity.none
    def fv: Set[String] = Set()
    def subexpressions: List[Expression] = List()
    def label = HOL

/**
 * A symbolic variable is like a regular SchemeVar, but also keeps track of its corresponding value using an address in the global store
 *
 * @param nam
 *   the name of the variable
 * @param adr
 *   the address of the value in the global store the symbolic representation corresponds to
 */
case class SymbolicVar(nam: String, adr: maf.core.Address) extends SchemeExp:
    def idn: Identity = Identity.none
    def fv: Set[String] = Set()
    def subexpressions: List[Expression] = List()
    def label = SVR

//
// For taint analysis
//

case class SchemeSource(name: Identifier, idn: Identity) extends SchemeExp:
    def fv: Set[String] = Set(name.toString)
    def label: Label = Label.SRC
    def subexpressions: List[Expression] = List(name)
    override val height: Int = name.height + 1
    override def toString: String = s"(source $name)"

case class SchemeSink(name: Identifier, idn: Identity) extends SchemeExp:
    def fv: Set[String] = Set(name.toString)
    def label: Label = Label.SNK
    def subexpressions: List[Expression] = List(name)
    override val height: Int = name.height + 1
    override def toString: String = s"(sink $name)"

case class SchemeSanitizer(name: Identifier, idn: Identity) extends SchemeExp:
    def fv: Set[String] = Set(name.toString)
    def label: Label = Label.SAN
    def subexpressions: List[Expression] = List(name)
    override val height: Int = name.height + 1
    override def toString: String = s"(sanitize $name)"
