package maf.modular.scv

import maf.modular.scheme.PrmAddr
import maf.core.Position.*
import maf.core.*
import maf.core.Monad.MonadIterableOps
import maf.core.Monad.MonadSyntaxOps
import maf.util.CollectionUtils.*
import maf.language.scheme.*
import maf.language.symbolic.*

trait UnstableComponentsWidening extends BaseScvBigStepSemantics with ScvContextSensitivity:

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
    private def isSmaller(c1: Component, c2: Component): Boolean =
        (context(c1), context(c2)) match
            case (Some(k1: KPathCondition[_]), Some(k2: KPathCondition[_])) =>
                val s1 = k1.stoCache.values.map(Unstable.size)
                val s2 = k2.stoCache.values.map(Unstable.size)
                s1.zip(s2).count(_ < _) < s1.zip(s2).count(_ >= _)

            // if both do not contain a path condition then they are not smaller
            case _ => false

    /**
     * Determines whether the given set of components needs widening
     *
     * @param cs
     *   the set of seen components
     * @param nww
     *   a new component
     */
    def shouldWiden(cs: Set[Component], nww: KPathCondition[Value]): Option[List[KPathCondition[Value]]] =
        val candidates = cs
            .collect { case CPathCondition(k, c) =>
                (k, c)
            }
            .toList
            .sortWith { case ((_, c1), (_, c2)) => isSmaller(c1, c2) }
            .map(_._1)

        if (candidates.size >= minUnstableLength) then
            // first we need to compute the set of subsequences
            val subsequences = candidates.combinations(minUnstableLength).map(_ ++ List(nww))
            // a sequence of components is unstable if and only if there exists a subsequence that has an unstable
            // sequence of stores
            subsequences.find(subsequence => subsequence.map(_.stoCache).pointwise().values.exists(Unstable.isUnstable))
        else None

    /**
     * Widening operation on the given set of components
     *
     * @param cmps
     *   a list of components, ordered as such that they satisfy the unstable sequence condition
     * @return
     *   a list of symbolic changes that needs to be applied on the symbolic store and path condition
     */
    def widen[M[_]: Monad](cmps: List[KPathCondition[Value]], fresh: M[SchemeExp]): M[List[SymChange]] =
        // the last component is the component of which we will change the symbolic representations in the store
        val last: Map[Addr, SchemeExp] = cmps.last.stoCache

        // the components in the unstable sequence determine which expression must be changed
        cmps.map(_.stoCache)
            .pointwise()
            .map { case (k: Addr, v: List[Unstable.Tree]) => (k, Unstable.isUnstable(v)) }
            .filter(_._2) // only update those addresses that are unstable
            .map(_._1) // compute the set of changes
            .mapM(key =>
                last.get(key) match
                    case Some(s) => fresh.map(nww => List(SymReplace(s, nww)))
                    case _       => Monad[M].unit(List()) // no key in the last store, so we don't care
            )
            .map(_.flatten)

    /**
     * This implementation of the build context function optionally widens a context such that termination is obtained.
     *
     * @see
     *   KPathCondition for information about the contents of this context
     */
    override def buildCtx(symArgs: List[Option[SchemeExp]], rangeContract: Option[Value] = None): ContextBuilder =
        new ContextBuilder:
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
                                         stoCache
                    )
                    // See if the addition of the new context merits widening.
                    // For this, fetch the set of similar components first.
                    similarComponents = getSimilarComponents(clo)
                    // compute the (potentially) widened context
                    changes <- shouldWiden(similarComponents, ctx) match
                        case Some(unstableSequence) => widen[EvalM](unstableSequence, fresh)
                        case _                      => scvMonadInstance.unit(List())

                    // apply the changes on the store and path condition
                    cpc = gcPc.applyChanges(changes)
                    cStoCache = stoCache.map { case (k, v) =>
                        k -> changes.foldLeft(v)((e, change) => change.apply(Assertion(e)).asInstanceOf[Assertion].contents)
                    }.toMap

                    cStoCacheArgs = cStoCache
                        .filterKeys {
                            case PrmAddr(_) => false
                            case _          => true
                        }
                        .toMap
                        .values
                    // We might remove too much, but this means that it is less constrained
                    (finalPc, reindexChanges) = cpc.gc(cStoCacheArgs.toSet).reindexed
                    //_ = { println(s"before gc $cpc, after gc $finalPc") }
                    cStoCacheReindexed = cStoCache
                        .mapValues(vlu => reindexChanges.foldLeft(vlu)((vlu, change) => change.apply(Assertion(vlu)).asInstanceOf[Assertion].contents))
                        .toMap

                    // the final context consists of an optionally widened store cache
                    finalCtx = KPathCondition(
                      finalPc,
                      rangeContract,
                      List(), /* 0-cfa */
                      (changes ++ reindexChanges).distinct, /* potential widening */
                      Map(), /* sym-args in sto cache */
                      cStoCacheReindexed
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
                //_ <- effectful { println(s"cache ${stoCacheArgs}") }
                //debug: _ = println(s"injectCtx rewritten stoCache $stoCacheArgs")
                // put the lexical store cache in the context of this evaluation
                _ <- putStoreCache(cache ++ stoCacheArgs)
                _ <- putVars(context.vars)
                _ <- putPc(context.pathCondition)
            yield ()

trait ComponentContentSimilarity extends UnstableComponentsWidening:
    type SimilarityContent = ComponentContent
    protected def similarityContent(cmp: Component): SimilarityContent = content(cmp)
    protected def getSimilarComponents(clo: (SchemeLambdaExp, Environment[Address])): Set[Component] =
        similarComponents.get(Some(clo)).getOrElse(Set())

trait UnstableWideningWithMinimum(val minUnstableLength: Int) extends ComponentContentSimilarity
