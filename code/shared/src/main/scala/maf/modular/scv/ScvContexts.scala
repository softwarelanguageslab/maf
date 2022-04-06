package maf.modular.scv

import maf.core.*
import maf.core.Position.*
import maf.util.datastructures.ListOps.*
import maf.language.scheme.*
import maf.language.symbolic.*
import maf.modular.scheme.modf.*
import maf.core.IdentityMonad

/*
 * Soft contract verification uses the following forms of
 * context:
 *
 * - Contract call: if a function that is beinig monitored by a particular
 *   contract is called, the pre-condition and post-condition contracts
 *   are part of the context, and can be used during the analysis such
 *   that the preconditions can be assumed, and the post-condition
 *   can be checked, after the analysis of a particular component
 * - k-path conditions: during the analysis, the path conditions
 *    from the previous "k" components are added to the context
 *    to enable more precise symbolic information to be available during the
 *    analysis.
 */
sealed trait ScvContext[L]

/** Used when the function call is originating from applying a monitor */
case class ContractCallContext[L](domains: List[L], rangeContract: L, args: List[SchemeExp], idn: Identity) extends ScvContext[L]

trait ScvContextSensitivity extends SchemeModFSensitivity:
    type ComponentContext = ScvContext[Value]

    /**
     * Keeps track of the path conditions from the last k components
     *
     * @param pc
     *   a list of path conditions from the last k components
     * @param vars
     *   a list of variables used in the path conditions from the last k components
     * @param symbolic
     *   a mapping between names of variables and symbolic values that should be passed between components (also from the last k components)
     * @param changes
     *   a list of changes applied to obtain the current path condition
     * @param symArgs
     *   a list of (symbolic) arguments of the called function (TODO: this might not be a great place to add this)
     */
    // TODO: factor out rangeContract, because it is required for a sound implementation of scv, and cannot be disabled/enabled
    // for certain experiments.As such it is not part of a variable context.
    case class KPathCondition[L](
        pc: PathCondition,
        rangeContract: Option[L],
        callers: List[Position],
        changes: List[SymChange],
        symArgs: Map[String, SchemeExp],
        stoCache: Map[Addr, SchemeExp])
        extends ScvContext[L]:
        override def toString: String = s"KPathCondition($pc, $rangeContract, $changes)"
    //s"KPathCondition(rangeContract = $rangeContract, pc = ${pc}, sstore = ${stoCache}, changes = ${changes}, symARgs = ${symArgs})"
    case class NoContext[L]() extends ScvContext[L]

    def contractContext(cmp: Component): Option[ContractCallContext[Value]] = context(cmp).flatMap {
        case c: ContractCallContext[_] =>
            // safety: the ComponentContext is constrained to ScvContext[Value] (where
            // the type paremeter is invariant) which
            // means that ContractCallContext is always in L = Value otherwise
            // it does not type check. But unfortunately type parameters are erased
            // at runtime, and the isInstanceOf check cannot garuantee the type
            //  of the type parameter at runtime.
            Some(c.asInstanceOf[ContractCallContext[Value]])

        case _ => None
    }

    override def allocCtx(
        clo: (SchemeLambdaExp, Environment[Address]),
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext = NoContext() // contexts will be created by overriding them in the semantics

trait ScvKContextSensitivity extends ScvContextSensitivity with ScvModAnalysis with ScvUpdateCount:
    import evalM.*
    import maf.core.Monad.MonadSyntaxOps

    protected def k: Int
    protected def m: Int

    protected def usingContract[X](cmp: Component)(f: Option[(List[Value], Value, List[SchemeExp], Identity)] => X): X = contractContext(cmp) match
        case Some(context) => f(Some(context.domains, context.rangeContract, context.args, context.idn))
        case _             => f(None)

    protected def usingRangeContract[X](cmp: Component)(f: Option[Value] => X): X = context(cmp) match
        case Some(KPathCondition(_, rangeContractOpt, _, _, _, _)) => f(rangeContractOpt)
        case _                                                     => f(None)

    override def fromContext(cmp: Component): FromContext = context(cmp) match
        case Some(KPathCondition(pc, _, _, _, symArgs, lcache)) => // KPathCondition(ps, sstore, _, cmps, _, _, lcache)
            new FromContext:
                def pathCondition: PathCondition = pc
                def vars: List[String] =
                    symArgs.values
                        .flatMap(PathCondition.visit[IdentityMonad.Id, List[String]](_) {
                            case SchemeVar(name)       => Some(List(name.name))
                            case SchemeVarLex(name, _) => Some(List(name.name))
                            case _                     => None
                        })
                        .toList
                def symbolic: Map[String, Option[SchemeExp]] = symArgs.mapValues(Some(_)).toMap
                def lexStoCache: Map[Address, SchemeExp] = lcache
        case _ => EmptyContext

    override def buildCtx(symArgs: List[Option[SchemeExp]], rangeContract: Option[Value] = None): ContextBuilder =
        new ContextBuilder:
            def allocM(
                clo: (SchemeLambdaExp, Environment[Address]),
                args: List[Value],
                call: Position,
                caller: Component
              ): EvalM[ComponentContext] =
                val symbolic = clo._1 match
                    case SchemeLambda(_, prs, _, _, _)          => prs.map(_.name).zip(symArgs)
                    case SchemeVarArgLambda(_, prs, _, _, _, _) => prs.map(_.name).zip(symArgs.take(prs.length))

                // Lexical symbolic store cache
                val lcache = lexicalStoCaches(clo)

                // Compute the store cache that will be shared with the next component:
                // remove all addresses that have been updated more than once, as the stored cache will not be valid anymore
                val ccache = lcache.filterKeys(!isUpdated(_)).toMap

                // roots are the free variables of the function (if not changed) and the function arguments themselves
                val roots: Set[SchemeExp] = symArgs.flatten.toSet ++ ccache.values.toSet

                // m-cfa
                val nextCallers = (context(caller) match
                    case Some(k: KPathCondition[_]) =>
                        (call :: k.callers)
                    case _ => List(call)
                ).take(m)

                val symArgsMap = symbolic.collect { case (k, Some(v)) => (k, v) }.toMap

                for
                    pc <- getPc
                    // simplify the path condition such that it only contains expressions in the root set
                    result <- withFresh {
                        pc.simplify(roots, PathCondition.onlyVarsAllowed, fresh)
                    }
                    // get the new path condition and the changes performed to obtain the new path condition
                    (newPc, changes) = result
                    // apply the changes on the lexical store cache
                    newCcache = changes.foldLeft(ccache) { case (ccache, change) =>
                        ccache.map { case (k, v) =>
                            change.apply(Assertion(v)) match
                                case Assertion(v) => (k, v)
                        }.toMap
                    }
                    // apply the changes on the symbolic arguments as well
                    newSymArgs = changes.foldLeft(symArgsMap)((symArgs, change) =>
                        symArgs.map { case (k, v) =>
                            change.apply(Assertion(v)) match
                                case Assertion(v) => (k, v)
                        }.toMap
                    )
                //_ <- effectful { println(s"old: $pc, new: $newPc") }
                yield KPathCondition(newPc, rangeContract, nextCallers, changes.filterDuplicates, newSymArgs, newCcache)

trait ScvOneContextSensitivity(protected val m: Int) extends ScvKContextSensitivity:
    protected val k: Int = 1
