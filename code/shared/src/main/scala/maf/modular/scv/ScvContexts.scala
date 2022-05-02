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
     * An address to represent the nth argument of a function call in the store cache.
     *
     * To be replaced with the actual address when inserting the store cache in the context of the callee
     */
    case class ArgAddr(n: Int) extends Address:
        override def printable: Boolean = false
        override def idn: Identity = Identity.none

    /** A SymAddr is used to store a symbolic represnetation of a particular value on a particular address */
    case class SymAddr(cmp: Component, addr: Address) extends Address:
        override def printable: Boolean = true
        override def idn: Identity = Identity.none

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

    object CPathCondition:
        def unapply(a: Component): Option[(KPathCondition[Value], Component)] = context(a) match
            case Some(k: KPathCondition[Value]) => Some((k, a))
            case _                              => None

    // TODO: factor out rangeContract, because it is required for a sound implementation of scv, and cannot be disabled/enabled
    // for certain experiments.As such it is not part of a variable context.
    case class KPathCondition[L](
        pc: PathCondition,
        rangeContract: Option[L],
        callers: List[Position],
        changes: List[SymChange],
        symArgs: Map[String, SchemeExp],
        widened: Boolean)
        extends ScvContext[L]:
        override def toString: String = s"KPathCondition($pc, $rangeContract, $changes)"

        def reindexed: KPathCondition[L] =
            val thisChanges = Reindexer.computeRenaming(this.pc, Map(), this.changes)
            val thisReindexedPc = this.pc.applyChanges(thisChanges)
            val thisChangesReindex = this.changes.map(_.applyChanges(thisChanges))
            this.copy(pc = thisReindexedPc, changes = thisChangesReindex)

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

trait StoreAllocateSymbolicValues extends ScvContextSensitivity, ScvModAnalysis, ScvUpdateCount:
    /**
     * Trait that can be mixed in when we want to allocate the symbolic representation of function arguments, and the free variables of a function
     *
     * @param symArgs
     *   the list of arguments corresponding to the arguments of the function (in-order)
     */
    trait StoreAllocateContextBuilder(symArgs: List[Option[SchemeExp]]) extends ContextBuilder:
        override def beforeCall(cmp: Component, prs: List[Identifier], clo: (SchemeLambdaExp, Environment[Address])): EvalM[Unit] =
            effectful {
                // store allocate the symbolic arguments of the function
                prs.zip(0 until prs.size).zip(symArgs).foreach {
                    case ((par, i), Some(sym)) =>
                        writeAddr(
                          SymAddr(cmp, ArgAddr(i)),
                          lattice.setRight(lattice.nil, Set(sym))
                        )
                    case (_, _) => ()
                }
                // store allocate the free variables of the function
                val freeVarsStoCache = lexicalStoCaches(clo).filterKeys(k => !isUpdated(k))
                freeVarsStoCache.foreach { case (addr, sym) => writeAddr(SymAddr(cmp, addr), lattice.setRight(lattice.nil, Set(sym))) }
            }

    /** Retrieve the store cache for a particular component */
    def lookupStoCache(cmp: Component): Map[Address, Symbolic] =
        // TODO: make this less expensive, by having a hierarchical map instead of going over all addresses in the global store
        store
            .collect { case (SymAddr(`cmp`, addr), vlu) =>
                val sym = lattice.getRight(vlu).toList
                // assertion must be true otherwise we should have a different path
                assert(sym.size <= 1)
                (addr -> sym.headOption)
            }
            .collect { case (a, Some(sym)) =>
                (a -> sym)
            }
            .toMap

    override def fromContext(cmp: Component): FromContext = context(cmp) match
        case Some(k @ KPathCondition(pc, _, _, _, symArgs, _)) => // KPathCondition(ps, sstore, _, cmps, _, _, lcache)
            new FromContext:
                def pathCondition: PathCondition = pc
                def vars: List[String] =
                    SymbolicStore.variables(lexStoCache)

                def symbolic: Map[String, Option[SchemeExp]] = symArgs.mapValues(Some(_)).toMap
                val lexStoCache: Map[Address, SchemeExp] = lookupStoCache(cmp)
        case _ => EmptyContext

trait ScvKContextSensitivity extends ScvContextSensitivity with ScvModAnalysis with ScvUpdateCount:
    import evalM.*
    import maf.core.Monad.MonadSyntaxOps

    protected def k: Int
    protected def m: Int

    override def fromContext(cmp: Component): FromContext = context(cmp) match
        case Some(k @ KPathCondition(pc, _, _, _, symArgs, _)) => // KPathCondition(ps, sstore, _, cmps, _, _, lcache)
            new FromContext:
                def pathCondition: PathCondition = PathCondition.empty
                def vars: List[String] = List()

                def symbolic: Map[String, Option[SchemeExp]] = Map()
                val lexStoCache: Map[Address, SchemeExp] = Map()
        case _ => EmptyContext

    protected def usingContract[X](cmp: Component)(f: Option[(List[Value], Value, List[SchemeExp], Identity)] => X): X = contractContext(cmp) match
        case Some(context) => f(Some(context.domains, context.rangeContract, context.args, context.idn))
        case _             => f(None)

    protected def usingRangeContract[X](cmp: Component)(f: Option[Value] => X): X = context(cmp) match
        case Some(KPathCondition(_, rangeContractOpt, _, _, _, _)) => f(rangeContractOpt)
        case _                                                     => f(None)

    override def buildCtx(symArgs: List[Option[SchemeExp]], rangeContract: Option[Value] = None): ContextBuilder =
        new ContextBuilder:
            def allocM(
                clo: (SchemeLambdaExp, Environment[Address]),
                args: List[Value],
                call: Position,
                caller: Component
              ): EvalM[ComponentContext] =
                unit(KPathCondition(PathCondition.empty, rangeContract, List(), List(), Map(), false))

trait ScvOneContextSensitivity(protected val m: Int) extends ScvKContextSensitivity:
    protected val k: Int = 1
