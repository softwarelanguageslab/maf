package maf.modular.scv

import maf.language.symbolic.*
import maf.language.symbolic.lattices.*
import maf.modular.scheme.modf._
import maf.modular.scheme.SchemeDomain
import maf.language.scheme._
import maf.util.TaggedSet
import maf.core._
import maf.language.scheme.primitives.SchemePrimM
import maf.language.symbolic.Symbolic
import maf.lattice.interfaces.BoolLattice

/**
 * Provides the base Scheme semantics for soft contract verification.
 *
 * The actual contract semantics are implemented in traits extending from this base trait
 */
trait ScvBaseSemantics extends BigStepModFSemanticsT with SymbolicSchemeDomain { outer =>
    import TaggedSet.*
    import MonadStateT.*
    import Monad.MonadSyntaxOps

    case class State(
        env: Environment[Address],
        store: StoreCache,
        lstore: BasicStore[Addr, Value],
        pc: PathCondition,
        freshVar: Int,
        vars: List[String]):
        def extendPc(addition: SchemeExp) = this.copy(pc = pc.extend(addition))

    object State:
        def empty: State =
            State(env = BasicEnvironment(Map()), store = Map(), new BasicStore(content = Map()), pc = PathCondition(EmptyFormula), freshVar = 0, List())

    case class PostValue(symbolic: Option[Symbolic], value: Value)

    object PostValue {
        def noSymbolic(value: Value): PostValue = PostValue(None, value)
    }

    override type EvalM[X] = ScvEvalM[X]
    type Symbolic = SchemeExp
    type SymbolicSet[X] = TaggedSet[Symbolic, X]
    type ScvEvalM[X] = MonadStateT[State, SymbolicSet, X]
    type StoreCache = Map[Addr, Symbolic]

    /////////////////////////////////////////////////////
    // Operations using contexts
    /////////////////////////////////////////////////////

    /**
     * Based on the current state of the scvMonadInstance build a context for newly created components
     *
     * @param symArgs
     *   a list of arguments of the function call corresponding to the context of the component we are building
     * @param rangeContract
     *   an optional range contract
     */
    protected def buildCtx(symArgs: List[Option[SchemeExp]], rangeContract: Option[Value]): ContextBuilder =
        DefaultContextBuilder

    /////////////////////////////////////////////////////
    // Monads
    /////////////////////////////////////////////////////

    final lazy val scvMonadInstance: StateOps[State, ScvEvalM] = MonadStateT.stateInstance[State, SymbolicSet]

    def withEnvM[X](f: Environment[Address] => EvalM[Environment[Address]])(ev: EvalM[X]): EvalM[X] =
        import scvMonadInstance.{get, impure, put, withState}

        for
            s <- get
            oldEnv = s.env
            newEnv <- f(oldEnv)
            s1 <- get
            _ <- put(s1.copy(env = newEnv))
            result <- ev
            newSt <- get
            _ <- put(newSt.copy(env = oldEnv))
        yield result

    implicit val evalM = new TEvalM:
        import scvMonadInstance.{get, impure, put, withState}
        export scvMonadInstance._
        def getEnv: EvalM[Environment[Address]] = get.map(_.env)
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => EvalM[X]): EvalM[X] =
            withEnvM(f andThen unit)(ev)

        //def guard(bln: Boolean): EvalM[Unit] =
        //  if bln then unit(()) else mzero
        def mzero[X]: EvalM[X] = MonadStateT.lift(TaggedSet.empty)
        def merge[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] =
            // two programs paths are not merged together in Scv but are rather explorered seperately
            nondet(x, y)
        def fail[X](e: Error): EvalM[X] =
            // also ignore exception in Scv semantics
            //warn(s"encountered error $e")
            mzero

    /* MonadStateT((state) => {
          val xRes = x.run(state)
          val yRes = y.run(state)
          val xs = xRes.vs.map(_._2._1)
          val ys = xRes.vs.map(_._2._1)
          val newValue = (xs ++ ys).foldLeft(Lattice[X].bottom)((acc, el) => Lattice[X].join(acc, el))
        }) */

    //////////////////////////////////////////////////
    // Other useful operations on the ScvEvalM monad
    // ///////////////////////////////////////////////

    /** Lookup the given address in the store cache and return the associated symbolic information */
    protected def lookupCache(addr: Addr): ScvEvalM[Option[Symbolic]] =
        scvMonadInstance.get.map(_.store.get(addr))

    protected def flatten[X](ms: ScvEvalM[TaggedSet[Symbolic, X]]): ScvEvalM[(Option[Symbolic], X)] =
        ms.flatMap(ts => MonadStateT.lift(TaggedSet.extract(ts)))

    protected def flattenSet[X](ms: ScvEvalM[Set[X]]): ScvEvalM[X] =
        ms.flatMap(xs => MonadStateT.lift(TaggedSet(xs.map((None, _)))))

    /** Returns a computation that, when composed with other computions results in no computation at all ie. void >>= m == void */
    protected def void[X]: ScvEvalM[X] = MonadStateT.lift(TaggedSet.empty)

    /** Extracts the tag along with the value from a computation returning such a tagged value */
    protected def extract(computation: EvalM[Value]): EvalM[PostValue] =
        for
            vlu <- computation
            symbolic = lattice.getRight(vlu)
            postvlu <-
                if symbolic.size == 0 then scvMonadInstance.unit(PostValue(None, vlu))
                else nondets(symbolic.map(s => PostValue(Some(s), vlu)).map(scvMonadInstance.unit))
        yield postvlu

    /**
     * Extend the path condition in the current state, and propogate it.
     *
     * @param symb
     *   the symbolic expression to add to the path condition (as a conjunction)
     */
    protected def extendPc(symb: Symbolic): EvalM[Unit] =
        for
            st <- scvMonadInstance.get
            _ <- scvMonadInstance.put(st.extendPc(symb))
        yield ()

    protected def getPc: EvalM[PathCondition] =
        scvMonadInstance.get.map(_.pc)

    /** Replaces the current path condition with the one given as a parameter */
    protected def putPc(pc: PathCondition): EvalM[Unit] =
        for
            st <- scvMonadInstance.get
            _ <- scvMonadInstance.put(st.copy(pc = pc))
        yield ()

    /** Replaces the current store cache with the given new store cache */
    protected def putStoreCache(cache: StoreCache): EvalM[Unit] =
        for
            st <- scvMonadInstance.get
            _ <- scvMonadInstance.put(st.copy(store = cache))
        yield ()

    protected def getVars: EvalM[List[String]] =
        scvMonadInstance.get.map(_.vars)

    protected def getStoreCache: EvalM[StoreCache] =
        scvMonadInstance.get.map(_.store)

    /** Replaces the current set of variables that are in the path condition with the given list */
    protected def putVars(vars: List[String]): EvalM[Unit] =
        for
            st <- scvMonadInstance.get
            _ <- scvMonadInstance.put(st.copy(vars = vars, freshVar = vars.size))
        yield ()

    protected def withFresh[X](m: EvalM[X]): EvalM[X] =
        for
            oldSt <- scvMonadInstance.get
            result <- m
            newSt <- scvMonadInstance.get
            _ <- scvMonadInstance.put(newSt.copy(freshVar = oldSt.freshVar, vars = oldSt.vars))
        yield result

    /** Generates a fresh symbolic variable */
    protected def fresh: EvalM[SchemeExp] =
        for
            st <- scvMonadInstance.get
            _ <- scvMonadInstance.put(st.copy(freshVar = st.freshVar + 1, vars = s"x${st.freshVar}" :: st.vars))
        yield SchemeVar(Identifier(s"x${st.freshVar}", Identity.none))

    /** Executes both computations non-determinstically */
    protected def nondet[X](tru: EvalM[X], fls: EvalM[X]): EvalM[X] =
        nondets(Set(tru, fls))

    /** For executing a side-effecting computation within the Monad (delayed) */
    protected def effectful(c: => Unit): EvalM[Unit] = MonadStateT((state) =>
        c
        TaggedSet.taggedSetMonad.unit(((), state))
    )

    protected def trace[X](prefix: String)(x: X): EvalM[X] =
        effectful { println(s"==== trace $prefix: $x ====") } >>> scvMonadInstance.unit(x)

    /** Executes the given computations non-determinstically */
    protected def nondets[X](branches: EvalM[X]*): EvalM[X] =
        nondets(branches.toSet)

    protected def nondets[X](branches: Set[EvalM[X]]): EvalM[X] =
        MonadStateT((state) => TaggedSet.flatten(branches.map(_.run(state))))

    /** Executes the given computation with the current store */
    protected def usingStore[X](m: (BasicStore[Address, Value], StoreCache) => EvalM[X]): EvalM[X] =
        scvMonadInstance.get.map(st => (st.lstore, st.store)).flatMap(p => m(p._1, p._2))

    /** Converts a Scheme expression to a compatible Symbolic representation */
    protected def symbolic(e: SchemeExp | Symbolic): Symbolic = e match {
        case e: SchemeExp => e
    }

    protected def stripIdn(exp: SchemeExp): SchemeExp = exp match
        case SchemeVar(Identifier(x, _))         => SchemeVar(Identifier(x, Identity.none))
        case SchemeVarLex(Identifier(x, _), lex) => SchemeVarLex(Identifier(x, Identity.none), lex)
        case SchemeFuncall(fexp, fargs, _) =>
            SchemeFuncall(stripIdn(fexp), fargs.map(stripIdn), Identity.none)
        case SchemeValue(vlu, _) => SchemeValue(vlu, Identity.none)

    /** Tags the given value with the given Scheme expression */
    protected def tag(e: SchemeExp | Symbolic)(v: Value): EvalM[Value] =
        scvMonadInstance.unit(lattice.setRight(v, Set(e)))

    protected def tag(e: Option[Symbolic])(v: Value): EvalM[Value] =
        // only tag if è` is Some
        e match
            case Some(sym) => tag(sym)(v)
            case _         => scvMonadInstance.unit(v)

    /** Write a symbolic representation to the store cache */
    protected def writeSymbolic(addr: Addr)(e: Symbolic): EvalM[Symbolic] =
        for
            st <- scvMonadInstance.get
            cache <- scvMonadInstance.unit(st.store)
            _ <- scvMonadInstance.put(st.copy(store = cache.updated(addr, e)))
        yield e

    protected def optional[X](m: Option[EvalM[X]]): EvalM[Unit] = m match
        case Some(am) => am.flatMap(_ => scvMonadInstance.unit(()))
        case None     => scvMonadInstance.unit(())

    override def intraAnalysis(cmp: Component): BaseIntraAnalysis

    trait BaseIntraAnalysis extends BigStepModFIntraT:
        /** We override `writeAddr` so that symbolic information is not leaked in the global store (except for return values, if desired) */
        override def writeAddr(addr: Addr, value: Value): Boolean =
            super.writeAddr(addr, lattice.setRight(value, Set()))

        /** Extending the store means that we need to extend both the local and global store */
        def extendStoCache(a: Address, v: PostValue): EvalM[Unit] =
            for
                st <- scvMonadInstance.get
                _ <- scvMonadInstance.put(
                  st.copy(lstore = st.lstore.extend(a, v.value), store = v.symbolic.map(sym => st.store + (a -> sym)).getOrElse(st.store))
                )
                _ <- effectful { writeAddr(a, v.value) }
            yield ()

        given SchemePrimM[EvalM, Address, Value] with
            export scvMonadInstance._
            def fail[X](err: Error): EvalM[X] = void // TODO: register error
            def mbottom[X]: EvalM[X] = void
            def mjoin[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] = evalM.merge(x, y)
            def allocVar(idn: Identifier): EvalM[Address] =
                throw new Exception("not supported")

            def allocPtr(exp: SchemeExp): EvalM[Address] = scvMonadInstance.unit(interpreterBridge.pointer(exp))

            /**
             * Two addresses are potentially equal when their `equals` implementation says so. However, they can point to multiple concrete addresses
             * (since they are abstract), so the result is top, otherwise it is defintely not the same address
             */
            def addrEq: EvalM[MaybeEq[Address]] = scvMonadInstance.unit(
              new MaybeEq[Address]:
                  def apply[B: BoolLattice](a1: Address, a2: Address): B =
                      if a1 == a2 then BoolLattice[B].top else BoolLattice[B].inject(false)
            )

            /** Extend the store without a post value */
            def extendSto(a: Address, v: Value): EvalM[Unit] =
                extendStoCache(a, PostValue(None, v))

            /**
             * Store lookup will return the value from the local store (if available) and otherwise the value from the global store.
             *
             * The value fro the local store can be more precise than the value in the local store, since the local store is flow sensitive while the
             * global one is not. However, since the local store in this analysis is not threaded through the entire analysis, in the fixed point the
             * local store does not result in a flow sensitive analysis, but instead in a flow insentive one.
             */
            def lookupSto(a: Address): EvalM[Value] = usingStore { (store, cache) =>
                val value = store.lookup(a).getOrElse(readAddr(a))
                // also return the symbolic representation if one is available
                cache.get(a) match
                    case Some(symbolic) => tag(symbolic)(value)
                    case _              => scvMonadInstance.unit(value)
            }

            def updateSto(a: Address, v: Value): EvalM[Unit] = extendSto(a, v)

}
