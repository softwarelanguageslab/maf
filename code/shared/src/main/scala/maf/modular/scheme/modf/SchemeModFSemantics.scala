package maf.modular.scheme.modf

import maf.core.Position._
import maf.core._
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.language.sexp
import maf.modular._
import maf.modular.components.ContextSensitiveComponents
import maf.modular.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.language.CScheme.TID
import maf.util._

/** Base definitions for a Scheme MODF analysis. */
// TODO: Most of this can be factored out to SchemeSemantics
trait BaseSchemeModFSemantics
    extends ModAnalysis[SchemeExp]
    with GlobalStore[SchemeExp]
    with ReturnValue[SchemeExp]
    with SchemeDomain
    with ContextSensitiveComponents[SchemeExp]:

    // the environment in which the ModF analysis is executed
    type Env = Environment[Addr]
    def baseEnv: Env

    // In ModF, components are function calls in some context.
    // All components used together with this Scheme MODF analysis should be viewable as SchemeComponents.
    def view(cmp: Component): SchemeModFComponent

    lazy val mainBody = program
    def expr(cmp: Component): SchemeExp = body(cmp)
    def body(cmp: Component): SchemeExp = body(view(cmp))
    def body(cmp: SchemeModFComponent): SchemeExp = cmp match
        case Main       => mainBody
        case c: Call[_] => SchemeBody(c.lambda.body)

    type ComponentContent = Option[(SchemeLambdaExp, Environment[Address])]
    def content(cmp: Component) = view(cmp) match
        case Main       => None
        case c: Call[_] => Some(c.clo)
    def context(cmp: Component): Option[ComponentContext] = view(cmp) match
        case Main                                 => None
        case c: Call[ComponentContext] @unchecked => Some(c.ctx)

    /** Creates a new component, given a closure, context and an optional name. */
    def newComponent(call: Call[ComponentContext]): Component

    /** Creates a new context given a closure, a list of argument values and the position of the call site. */
    def allocCtx(
        clo: (SchemeLambdaExp, Environment[Address]),
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext

    /** Allocating addresses */
    type AllocationContext
    def allocVar(id: Identifier, cmp: Component): VarAddr[AllocationContext]
    def allocPtr(exp: SchemeExp, cmp: Component): PtrAddr[AllocationContext]

    /* convience accessors for argument values */
    def argValues(cmp: Component): Map[String, Value] = view(cmp) match
        case Main => Map.empty
        case Call((lam, _), _) =>
          val argAddrs = lam.args.map(id => (id.name, allocVar(id, cmp))).toMap
          val allArgAddrs = lam.varArgId match
              case None         => argAddrs
              case Some(varArg) => argAddrs + (varArg.name -> allocVar(varArg, cmp))
          allArgAddrs.map(bnd => (bnd._1, store.getOrElse(bnd._2, lattice.bottom)))

    /* convience accessor for getting the argument values in the order of their definition */
    def argValuesList(cmp: Component): List[(Addr, Value)] = view(cmp) match
        case Main => List()
        case Call((lam, _), _) =>
          val argAddrs = lam.args.map(id => allocVar(id, cmp))
          val allArgAddrs = lam.varArgId match
              case None         => argAddrs
              case Some(varArg) => argAddrs ++ List(allocVar(varArg, cmp))

          allArgAddrs.map(addr => (addr, store.getOrElse(addr, lattice.bottom)))

    //XXXXXXXXXXXXXXXXXXXXXXXXXX//
    // INTRA-COMPONENT ANALYSIS //
    //XXXXXXXXXXXXXXXXXXXXXXXXXX//

    // Extensions to the intraAnalysis.
    override def intraAnalysis(cmp: Component): SchemeModFSemanticsIntra
    trait SchemeModFSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra { modf =>
      // components
      protected def fnBody: SchemeExp = body(view(component))
      protected def fnEnv: Env = view(component) match
          case Main => baseEnv
          case c: Call[_] =>
            val extEnv = c.env.extend(c.lambda.args.map { id =>
              (id.name, allocVar(id, component))
            })
            c.lambda.varArgId match
                case None         => extEnv
                case Some(varArg) => extEnv.extend(varArg.name, allocVar(varArg, component))
      // variable lookup: use the global store
      protected def lookup(id: Identifier, env: Env): Value = env.lookup(id.name) match
          case None       => throw new Exception(s"Undefined variable $id (${id.idn.pos})") //TODO: better error reporting
          case Some(addr) => readAddr(addr)
      protected def assign(
          id: Identifier,
          env: Env,
          vlu: Value
        ): Unit = env.lookup(id.name) match
          case None       => throw new Exception(s"Undefined variable $id (${id.idn.pos})") //TODO: better error reporting
          case Some(addr) => writeAddr(addr, vlu)
      protected def assign(bds: List[(Identifier, Value)], env: Env): Unit =
        bds.foreach { case (id, vlu) => assign(id, env, vlu) }
      protected def bind(
          id: Identifier,
          env: Env,
          vlu: Value
        ): Env =
          val addr = allocVar(id, component)
          val env2 = env.extend(id.name, addr)
          writeAddr(addr, vlu)
          env2
      protected def bind(bds: List[(Identifier, Value)], env: Env): Env =
        bds.foldLeft(env)((env2, bnd) => bind(bnd._1, env2, bnd._2))
      protected def applyFun(
          fexp: SchemeFuncall,
          fval: Value,
          args: List[(SchemeExp, Value)],
          cll: Position,
          ctx: Option[(() => ComponentContext)] = None,
        ): Value =
          val fromClosures = applyClosures(fval, args, cll, ctx)
          val fromPrimitives = applyPrimitives(fexp, fval, args)
          applyContinuations(fval, args)
          lattice.join(fromClosures, fromPrimitives)
      private def applyContinuations(k: Value, args: List[(SchemeExp, Value)]) =
        args match
            case (_, vlu) :: Nil =>
              val cnts = lattice.getContinuations(k)
              cnts.foreach(cnt => writeResult(vlu, cnt.asInstanceOf[Component])) // TODO: type safety!
            case _ => () // continuations are only called with a single argument
      // => ignore continuation calls with more than 1 argument
      // TODO[minor]: use foldMap instead of foldLeft
      protected def applyClosures(
          fun: Value,
          args: List[(SchemeExp, Value)],
          cll: Position,
          ctx: Option[(() => ComponentContext)] = None,
        ): Value =
          val arity = args.length
          val closures = lattice.getClosures(fun)
          closures.foldLeft(lattice.bottom)((acc, clo) =>
            lattice.join(
              acc,
              clo match {
                case (SchemeLambda(_, prs, _, _), _) if prs.length == arity =>
                  val argVals = args.map(_._2)
                  val context = ctx.map(_.apply()) getOrElse (allocCtx(clo, argVals, cll, component))
                  val targetCall = Call(clo, context)
                  val targetCmp = newComponent(targetCall)
                  bindArgs(targetCmp, prs, argVals)
                  call(targetCmp)
                case (SchemeVarArgLambda(_, prs, vararg, _, _), _) if prs.length <= arity =>
                  val (fixedArgs, varArgs) = args.splitAt(prs.length)
                  val fixedArgVals = fixedArgs.map(_._2)
                  val varArgVal = allocateList(varArgs)
                  val context = ctx.map(_.apply()) getOrElse allocCtx(clo, fixedArgVals :+ varArgVal, cll, component)
                  val targetCall = Call(clo, context)
                  val targetCmp = newComponent(targetCall)
                  bindArgs(targetCmp, prs, fixedArgVals)
                  bindArg(targetCmp, vararg, varArgVal)
                  call(targetCmp)
                case _ => lattice.bottom
              }
            )
          )
      protected def allocateList(elms: List[(SchemeExp, Value)]): Value = elms match
          case Nil                => lattice.nil
          case (exp, vlu) :: rest => allocateCons(exp)(vlu, allocateList(rest))
      protected def allocateCons(pairExp: SchemeExp)(car: Value, cdr: Value): Value =
        allocateVal(pairExp)(lattice.cons(car, cdr))
      protected def allocateString(stringExp: SchemeExp)(str: String): Value =
        allocateVal(stringExp)(lattice.string(str))
      protected def allocateVal(exp: SchemeExp)(v: Value): Value =
          val addr = allocPtr(exp, component)
          writeAddr(addr, v)
          lattice.pointer(addr)
      protected def append(appendExp: SchemeExp)(l1: (SchemeExp, Value), l2: (SchemeExp, Value)): Value =
        //TODO [difficult]: implement append
        throw new Exception("NYI -- append")
      private def bindArg(
          component: Component,
          par: Identifier,
          arg: Value
        ): Unit =
        writeAddr(allocVar(par, component), arg)

      private def bindArgs(
          component: Component,
          pars: List[Identifier],
          args: List[Value]
        ): Unit =
        pars.zip(args).foreach { case (par, arg) => bindArg(component, par, arg) }

      protected def currentThread: TID =
        throw new Exception("Concurrency not available in ModF")
      given interpreterBridge: SchemeInterpreterBridge[Value, Addr] with
          def pointer(exp: SchemeExp): Addr = allocPtr(exp, component)
          def readSto(adr: Addr): Value = readAddr(adr)
          def writeSto(adr: Addr, vlu: Value) = writeAddr(adr, vlu)
          def callcc(
              clo: (SchemeLambdaExp, Environment[Address]),
              fpos: Position
            ): Value = modf.callcc(clo, fpos)
          def currentThread = modf.currentThread
      // TODO[minor]: use foldMap instead of foldLeft
      protected def applyPrimitives(
          fexp: SchemeFuncall,
          fval: Value,
          args: List[(SchemeExp, Value)]
        ): Value =
        lattice
          .getPrimitives(fval)
          .foldLeft(lattice.bottom)((acc, prm) =>
            lattice.join(
              acc,
              primitives(prm).callMF(fexp, args.map(_._2)) match {
                case MayFailSuccess(vlu) => vlu
                case MayFailBoth(vlu, _) => vlu
                case MayFailError(_)     => lattice.bottom
              }
            )
          )
      // evaluation helpers
      protected def evalLiteralValue(literal: sexp.Value, exp: SchemeExp): Value = literal match
          case sexp.Value.Integer(n)   => lattice.number(n)
          case sexp.Value.Real(r)      => lattice.real(r)
          case sexp.Value.Boolean(b)   => lattice.bool(b)
          case sexp.Value.String(s)    => allocateString(exp)(s)
          case sexp.Value.Character(c) => lattice.char(c)
          case sexp.Value.Symbol(s)    => lattice.symbol(s)
          case sexp.Value.Nil          => lattice.nil
      // The current component serves as the lexical environment of the closure.
      protected def newClosure(
          lambda: SchemeLambdaExp,
          env: Env
        ): Value =
        lattice.closure((lambda, env.restrictTo(lambda.fv)))

      protected def callcc(
          closure: (SchemeLambdaExp, Environment[Address]),
          fpos: Position
        ): Value =
          val ctx = allocCtx(closure, Nil, fpos, component)
          val cll = Call(closure, ctx)
          val cmp = newComponent(cll)
          val cnt = lattice.cont(cmp)
          val par = closure._1.args.head
          bindArg(cmp, par, cnt)
          call(cmp)

      // other helpers
      protected def conditional[M: Monoid](
          prd: Value,
          csq: => M,
          alt: => M
        ): M =
          val csqVal = if lattice.isTrue(prd) then csq else Monoid[M].zero
          val altVal = if lattice.isFalse(prd) then alt else Monoid[M].zero
          Monoid[M].append(csqVal, altVal)
    }

trait SchemeModFSemantics extends SchemeSetup with BaseSchemeModFSemantics with StandardSchemeModFAllocator:
    def baseEnv = initialEnv

// for convenience, since most Scheme analyses don't need this much parameterization
abstract class SimpleSchemeModFAnalysis(prg: SchemeExp)
    extends ModAnalysis[SchemeExp](prg)
    with StandardSchemeModFComponents
    with SchemeModFSemantics
    with BigStepModFSemantics:
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
