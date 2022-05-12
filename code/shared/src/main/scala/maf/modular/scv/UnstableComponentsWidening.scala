package maf.modular.scv

import maf.modular.scheme.PrmAddr
import maf.core.Position.*
import maf.core.*
import maf.core.Monad.MonadIterableOps
import maf.core.Monad.MonadSyntaxOps
import maf.util.CollectionUtils.*
import maf.language.scheme.*
import maf.language.symbolic.*

trait UnstableComponentsWidening extends BaseScvBigStepSemantics with ScvContextSensitivity with StoreAllocateSymbolicValues:

    type SimilarityContent

    /** The minimal length of an unstable subsequence */
    def minUnstableLength: Int

    protected def similarityContent(cmp: Component): SimilarityContent

    /**
     * A map from similarity determinants, to the actual components.
     *
     * This is used to determine a number of components, and to find a sequence in them that satisfies the unstable sequence condition.
     */
    protected var similarComponents: Map[SimilarityContent, Set[Component]] = Map()

    /** Fetches a set of similar components based on the closure of the component */
    protected def getSimilarComponents(clo: (SchemeLambdaExp, Environment[Address])): Set[Component]

    /** Override spawn such that it keeps track of the similar components */
    override def spawn(cmp: Component): Unit =
        similarComponents = similarComponents + (similarityContent(cmp) -> (similarComponents.get(similarityContent(cmp)).getOrElse(Set()) + cmp))
        super.spawn(cmp)

    /**
     * A component C is smaller than another component C' if the number of smaller expressions according to maf.language.symbolic.Unstable.size is
     * smaller than the number of smaller expressions in C'.
     */
    protected def isSmaller: Ordering[Map[Address, Symbolic]] =
        new scala.math.Ordering[Map[Address, Symbolic]]:
            def compare(k1: Map[Address, Symbolic], k2: Map[Address, Symbolic]): Int =
                if k1 == k2 then 0
                else
                    val s1 = k1.values.map(Unstable.size)
                    val s2 = k2.values.map(Unstable.size)
                    s1.zip(s2).count(_ < _).compare(s1.zip(s2).count(_ >= _))

    /**
     * Determines whether the given set of components needs widening
     *
     * @param cs
     *   the set of seen components
     * @param nww
     *   a new component
     */
    def shouldWiden(cs: Set[Component], stoCache: Map[Address, Symbolic]): Option[List[Component]] =
        val init = cs.collect { case CPathCondition(k, c) =>
            (k, c, lookupStoCache(c))
        }.toList

        val candidates = init.sortBy { case (_, _, stoCache) => stoCache }(isSmaller)

        if (candidates.size >= minUnstableLength) then
            // first we need to compute the set of subsequences
            val subsequences = candidates.combinations(minUnstableLength)
            // a sequence of components is unstable if and only if there exists a subsequence that has an unstable
            // sequence of stores
            subsequences.find(subsequence => (subsequence.map(_._3) ++ List(stoCache)).pointwise().values.exists(Unstable.isUnstable)).map(_.map(_._2))
        else None

    def widen[M[_]: Monad](
        cmps: List[Component],
        fresh: M[SchemeExp],
        ctx: KPathCondition[Value],
        clo: (SchemeLambdaExp, Env),
        stoCache: Map[Address, Symbolic],
      ): M[KPathCondition[Value]]

    /**
     * This implementation of the build context function optionally widens a context such that termination is obtained.
     *
     * @see
     *   KPathCondition for information about the contents of this context
     */

    abstract class ContextBuilderWithWidening(symArgs: List[Option[Symbolic]], rangeContract: Option[Value])
        extends StoreAllocateContextBuilder(symArgs):
        def allocM(
            clo: (SchemeLambdaExp, Environment[Address]),
            args: List[Value],
            call: Position,
            caller: Component
          ): EvalM[ComponentContext] =

            // construct the contents of the store cache based on the arguments of the called function
            val stoCacheArgs = symArgs.take(clo._1.args.size).zip(0 until symArgs.size).foldLeft(Map[Addr, SchemeExp]()) {
                case (cache, (Some(v), i)) => cache + (ArgAddr(i) -> v)
                case (cache, _)            => cache
            }

            // also add the free variables to the store cache, except for those that have been updated
            val stoCache = stoCacheArgs ++ lexicalStoCaches(clo).filterKeys(k => !isUpdated(k) && !k.isInstanceOf[ArgAddr]).toMap
            // A temporary (non-wided) version of the context, this must be in the monad to retrieve the path condition
            for
                //_ <- effectful { println("computing the context") }
                pc <- getPc
                // we will already clean up the path condition to only include information in the current stoCache
                gcPc = pc.gc(stoCache.values.toSet)
                //debug: _ = { println(s"before clean $pc, after clean $gcPc") }
                //debug: _ = { println(s"sto cache $stoCache") }
                // create a non-widened version of context
                ctx = KPathCondition(gcPc,
                                     rangeContract,
                                     List() /* 0-cfa */,
                                     List() /* no widening, so no changes */,
                                     Map() /* sym-args in stoCache */,
                                     false
                )
                // See if the addition of the new context merits widening.
                // For this, fetch the set of similar components first.
                similarComponents = getSimilarComponents(clo)
                // compute the (potentially) widened context
                widenedCtx <- shouldWiden(similarComponents, stoCache) match
                    case Some(unstableSequence) =>
                        widen[EvalM](unstableSequence, fresh, ctx, clo, stoCache)
                    case _ => scvMonadInstance.unit(ctx)

                // We might remove too much, but this means that it is less constrained
                (finalPc, reindexChanges) = (widenedCtx.pc.gc(stoCache.values.toSet), List[SymChange]())

                // the final context consists of an optionally widened store cache
                finalCtx = KPathCondition(
                  finalPc,
                  rangeContract,
                  List(), /* 0-cfa */
                  (widenedCtx.changes ++ reindexChanges).distinct, /* potential widening */
                  Map(), /* sym-args in sto cache */
                  widenedCtx.widened
                )
            //_ <- effectful { println("done computing the context") }
            yield finalCtx

    override def intraAnalysis(component: Component): IntraWidening

    trait IntraWidening extends BaseIntraScvSemantics:
        /** We override injectCtx such that we can change how the store cache is inserted */
        override def injectCtx: EvalM[Unit] =
            //debug: println(s"injectCtx: unstable widening with n=${minUnstableLength}")
            val context = fromContext(cmp)
            //println(s"injectCtx: $cmp")
            // compute the addresses of the arguments
            val args = fnArgs
            //
            for
                cache <- getStoreCache

                // add the actual addresses of the arguments in the store cache
                stoCacheArgs = context.lexStoCache.map {
                    case (ArgAddr(i), sym) => args(i) -> sym
                    case (addr, sym)       => (addr -> sym)
                }.toMap
                //debug: _ = println(s"injectCtx rewritten stoCache $stoCacheArgs")
                // put the lexical store cache in the context of this evaluation
                _ <- putStoreCache(cache ++ stoCacheArgs)
                _ <- putVars(SymbolicStore.variables(stoCacheArgs) ++ context.vars)
                st <- scvMonadInstance.get
                _ <- putPc(context.pathCondition)
            yield ()

trait ComponentContentSimilarity extends UnstableComponentsWidening:
    type SimilarityContent = ComponentContent
    protected def similarityContent(cmp: Component): SimilarityContent = content(cmp)
    protected def getSimilarComponents(clo: (SchemeLambdaExp, Environment[Address])): Set[Component] =
        similarComponents.get(Some(clo)).getOrElse(Set())

/** Upon encountering a component that should be widened, does not widen it at all */
trait UnstableComponentWideningNoWidening extends UnstableComponentsWidening:
    override def shouldWiden(cs: Set[Component], stoCache: Map[Address, Symbolic]): Option[List[Component]] =
        // since we don't do any widening, we won't singal the need for widening
        None

    def widen[M[_]: Monad](
        cmps: List[Component],
        fresh: M[SchemeExp],
        ctx: KPathCondition[Value],
        clo: (SchemeLambdaExp, Env),
        stoCache: Map[Address, Symbolic]
      ): M[KPathCondition[Value]] = Monad[M].unit(ctx)

/**
 * Widens by removing the path condition entirely (expect for expressions that **only** contain free variables), and by only keeping non-arguments in
 * the symbolic store.
 *
 * This is equivalent to the Nguyen approach from their 2018 AAM based implementation.
 */
trait RemovePathCondition extends UnstableComponentsWidening:
    override def buildCtx(symArgs: List[Option[SchemeExp]], rangeContract: Option[Value] = None, contractCall: Boolean): ContextBuilder =
        new ContextBuilderWithWidening(symArgs, rangeContract) {
            override def beforeCall(cmp: Component, prs: List[Identifier], clo: (SchemeLambdaExp, Environment[Address])): EvalM[Unit] = context(
              cmp
            ) match
                case Some(k: KPathCondition[_]) if !k.widened => super.beforeCall(cmp, prs, clo)
                case _                                        => scvMonadInstance.unit(())
        }

    def widen[M[_]: Monad](
        cmps: List[Component],
        fresh: M[SchemeExp],
        ctx: KPathCondition[Value],
        clo: (SchemeLambdaExp, Env),
        stoCache: Map[Address, Symbolic]
      ): M[KPathCondition[Value]] =
        val currentCtx = ctx
        // we don't care about the pc and stoCache of the current context, we simply fetch pc and cache from lexical scope of called closure
        val lexPc = lexicalPathConditions(clo)
        val lexStoCache = lexicalStoCaches(clo)
        // we must filter the store cache such that it substitutes fresh variables for updated ones
        for
            updatedStoCache <- lexStoCache
                .mapM {
                    case (k, v) if isUpdated(k) => fresh.map(k -> _)
                    case (k, v)                 => Monad[M].unit((k -> v))
                }
                .map(_.toMap)
        yield KPathCondition(lexPc, currentCtx.rangeContract, currentCtx.callers, currentCtx.changes, currentCtx.symArgs, true)

trait UnstableWideningWithMinimum(val minUnstableLength: Int) extends ComponentContentSimilarity
