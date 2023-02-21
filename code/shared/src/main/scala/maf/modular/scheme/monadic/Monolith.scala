package maf.modular.scheme.monadic

import maf.core.*
import maf.core.Monad.*
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.modular.scheme.SchemeDomain
import maf.core.worklist.FIFOWorkList
import maf.modular.Dependency
import maf.modular.AddrDependency
import maf.modular.ReturnAddr
import maf.lattice.interfaces.BoolLattice
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.language.scheme.SchemeExp
import maf.language.scheme.SchemeBegin
import scala.io.StdIn
import maf.util.Default

trait Suspend:
    type State
    sealed trait Suspend[+T]:
        def continue: Suspend[T] = sys.error("cannot continue current suspend")
        def run: T = this match
            case Done(v)           => v
            case _: SuspendInfo[_] => this.continue.run

    case class Done[T](v: T) extends Suspend[T]
    sealed trait SuspendInfo[A]:
        def state: State
        def v: A

    case class Suspended[A](state: State, v: A) extends Suspend[A], SuspendInfo[A]
    case class SuspendedFlatMap[A, B](state: State, v: A, next: A => Suspend[B]) extends Suspend[B], SuspendInfo[A]:
        override def continue: Suspend[B] = next(v)

    given suspendMonad: Monad[Suspend] with
        def map[X, Y](m: Suspend[X])(f: X => Y): Suspend[Y] =
            flatMap(m)(f andThen unit)

        def flatMap[X, Y](m: Suspend[X])(f: X => Suspend[Y]): Suspend[Y] = m match
            case Done(v)                   => f(v)
            case s: Suspended[X]           => SuspendedFlatMap[X, Y](s.state, s.v, f)
            case s: SuspendedFlatMap[_, _] => SuspendedFlatMap(s.state, s.v, (v) => s.next(v).flatMap(f))
            case _                         => sys.error("unreachable")

        def unit[X](x: X): Suspend[X] = Done(x)

    def suspend[T: Default](st: State): Suspend[T] =
        Suspended(st, Default.default)

object SuspendTest extends Suspend:
    import suspendMonad.*

    type State = Int
    def times(a: Int, b: Int): Suspend[Int] =
        if a > 0 then
            for
                _ <- suspend[Unit](a)
                res <- times(a - 1, b)
                _ <- suspend[Unit](res)
            yield res + b
        else unit(0)

    def main(args: Array[String]): Unit =
        def loop(s: Suspend[Int]): Int =
            println(s"current s $s")
            s match
                case Done(v) => v
                case s: SuspendInfo[_] =>
                    println(s"Current state: ${s.state}")
                    println("Continue (c) or quit (q)")
                    val choice = StdIn.readLine
                    if choice == "c" then loop(s.continue)
                    else -1

        println(s"result ${loop(times(4, 3))}")

trait Monolith extends SchemeSemantics:
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    //
    // Type aliases (for convience)
    //

    type Sto = BasicStore[Address, Value]

    //
    // Components
    //

    sealed trait Component
    case class Call(lam: Lam, env: Env, ctx: Ctx) extends Component
    case object Main extends Component

    //
    // State
    //

    /**
     * Internal state to keep track of ModF effects
     *
     * @param sto
     *   the global store
     * @param W
     *   triggered dependencies (writes)
     * @param R
     *   dependencies (reads)
     * @param C
     *   spawns (calls)
     */
    case class Effects(
        cmp: Component,
        sto: Sto,
        wl: FIFOWorkList[Component] = FIFOWorkList.empty,
        seen: Set[Component] = Set(),
        W: Set[Dependency] = Set(),
        R: Map[Dependency, Set[Component]] = Map(),
        C: Set[Component] = Set()):
        def merge(other: Effects): Effects =
            this.copy(sto = this.sto.extend(other.sto.content.toIterable), W = this.W ++ other.W, R = this.R ++ other.R, C = this.C ++ other.C)

    // EffectT monad

    sealed trait SuspendM[T]:
        /** Runs the monad with the given configuration if it is not suspended */
        def runNext(env: Env, ctx: Ctx, state: Effects): SuspendM[T]

        /** Changes the state of the monad with the given values, also changes the saved suspended state */
        def withState(env: Env, ctx: Ctx, state: Effects): SuspendM[T]

        /** Resumes the suspended computation and returns the next suspended computation */
        def resume: SuspendM[T] = sys.error(s"computation $this cannot be resumed")

        /** Resumes the suspended computation and passes control to the given continuation */
        def resumeWith[B](cnt: T => SuspendM[B]): SuspendM[B] = sys.error(s"computation $this cannot be resumed")

        def getState: (Env, Ctx, Effects) = sys.error(s"no state for $this")

    case class Done[A](v: (Effects, Option[A])) extends SuspendM[A]:
        override def toString: String = s"<done ${v._2}>"
        def runNext(env: Env, ctx: Ctx, state: Effects): SuspendM[A] = this
        override def resume: SuspendM[A] = this
        def withState(env: Env, ctx: Ctx, state: Effects): SuspendM[A] =
            /* nothing to change here */
            Done((state, v._2))
    case class Next[A](run: (Env, Ctx, Effects) => SuspendM[A]) extends SuspendM[A]:
        def runNext(env: Env, ctx: Ctx, state: Effects): SuspendM[A] =
            run(env, ctx, state).runNext(env, ctx, state)

        def withState(env: Env, ctx: Ctx, state: Effects): SuspendM[A] =
            Next((_, _, _) => run(env, ctx, state))
    case class Suspended[A](state: (Env, Ctx, Effects), vlu: A) extends SuspendM[A]:
        override def toString(): String = s"<suspended $vlu, ${state._3.cmp}>"
        def runNext(env: Env, ctx: Ctx, state: Effects): SuspendM[A] = this
        def withState(env: Env, ctx: Ctx, state: Effects): SuspendM[A] =
            Suspended((env, ctx, state), vlu)
        override def resumeWith[B](cnt: A => SuspendM[B]): SuspendM[B] =
            cnt(vlu).runNext.tupled(state)

    /** Represents suspendable subclasses of the SuspendM monad (marker trait) */
    trait Suspendable[A] extends SuspendM[A]

    case class FlatMap[A, T](current: SuspendM[A], next: A => SuspendM[T]) extends SuspendM[T], Suspendable[T]:
        override def toString(): String = s"[[$current >>= $next]]"
        def runNext(env: Env, ctx: Ctx, state: Effects): SuspendM[T] = this
        def withState(env: Env, ctx: Ctx, state: Effects): SuspendM[T] =
            FlatMap(current.withState(env, ctx, state), next)
        override def resume: SuspendM[T] = current.resumeWith(next)

        override def getState: (Env, Ctx, Effects) = current match
            case Suspended(state, vlu) =>
                state
            case _ => ???

    case class Fork[A](lattice: Lattice[A], initialState: (Env, Ctx, Effects), csq: SuspendM[A], alt: SuspendM[A]) extends SuspendM[A], Suspendable[A]:
        override def toString(): String = s"<fork $csq ++ $alt>"
        def runNext(env: Env, ctx: Ctx, state: Effects): SuspendM[A] = this
        def withState(env: Env, ctx: Ctx, st: Effects): SuspendM[A] =
            Fork(lattice, initialState, csq.withState(env, ctx, st), alt.withState(env, ctx, st))
        override def resume: SuspendM[A] = (csq, alt) match
            case (Done(st1, v1), Done(st2, v2)) => Done(st1.merge(st2), merge(v1, v2)(using lattice))
            case (_, _)                         => Fork(lattice, initialState, csq.resume, alt.resume)
        override def resumeWith[B](cnt: A => SuspendM[B]): SuspendM[B] =
            (csq, alt) match
                case (Done((st1, v1)), Done((st2, v2))) =>
                    me.flatMap(Done(st1.merge(st2), merge(v1, v2)(using lattice)))(cnt).runNext(initialState._1, initialState._2, st1.merge(st2))
                case (_, _) => me.flatMap(Fork(lattice, initialState, csq.resume, alt.resume))(cnt)

    object SuspendM:
        /** Run the given suspendable computation until completion */
        def run[A](m: SuspendM[A])(env: Env, ctx: Ctx, state: Effects): (Effects, Option[A]) =
            var ctr = 0
            def loop(current: SuspendM[A]): (Effects, Option[A]) =
                ctr = ctr + 1
                println(s"iteration $ctr\n==============\n$current")
                current match
                    case Done(v) => v
                    case s: Suspendable[_] =>
                        loop(s.resume)
                    case _ => sys.error(s"cannot run on $current")

            loop(m.runNext(env, ctx, state))

    given me: Monad[SuspendM] with
        def unit[X](x: X): SuspendM[X] = Next((env, ctx, state) => Done((state, Some(x))))
        def flatMap[X, Y](m: SuspendM[X])(f: X => SuspendM[Y]): SuspendM[Y] = m match
            case Done((state, value)) =>
                Next((env, ctx, _) => value.map(f andThen (_.withState(env, ctx, state))).getOrElse(Done(state, None)))
            case Next(run) =>
                Next((env, ctx, state) => run(env, ctx, state).flatMap(f).runNext(env, ctx, state))
            case FlatMap(m, e) =>
                FlatMap(m, (v) => e(v).flatMap(f))
            case s @ Suspended(state, vlu) =>
                FlatMap(s, f)
            //case current @ FlatMap(_, _) =>
            //    FlatMap(current, f)
            case fork @ Fork(_, _, _, _) =>
                FlatMap(fork, f)

        def map[X, Y](m: SuspendM[X])(f: X => Y): SuspendM[Y] =
            flatMap(m)(f andThen unit)

    private def currentCmp[M[_]: Monad]: SuspendM[Component] =
        Next((_, _, e) => Done(e, Some(e.cmp)))

    private def modify(f: Effects => Effects): SuspendM[Unit] =
        Next((_, _, e) => Done(f(e), Some(())))

    private def spawn(cmp: Component): SuspendM[Unit] =
        modify(e => e.copy(C = e.C + cmp))

    private def read(adr: Address): SuspendM[Value] =
        val dep = AddrDependency(adr)
        val r = (e: Effects) => (dep -> (e.R.get(dep).getOrElse(Set()) + e.cmp))
        modify(e => e.copy(R = e.R + r(e))) >>> get.map(_.sto.lookup(adr).getOrElse(lattice.bottom))

    private def write(adr: Address, v: Value): SuspendM[Unit] =
        modify(e =>
            if e.sto.lookup(adr).getOrElse(lattice.bottom) == v then e
            else e.copy(W = e.W + AddrDependency(adr), sto = e.sto.extend(adr, v))
        )

    private def get: SuspendM[Effects] =
        Next((_, _, e) => Done(e, Some(e)))

    protected def getAllState: SuspendM[(Env, Ctx, Effects)] =
        Next((env, ctx, eff) => Done(eff, Some((env, ctx, eff))))

    protected def merge[A](x: Option[A], y: Option[A])(using lat: Lattice[A]): Option[A] = (x, y) match
        case (Some(v1), Some(v2)) => Some(lat.join(v1, v2))
        case (Some(v1), None)     => Some(v1)
        case (None, Some(v2))     => Some(v2)
        case _                    => None

    /** Merge two suspendable computations together, this method can be overriden to change the order in which they are suspended. */
    protected def merge[A](x: SuspendM[A], y: SuspendM[A])(using lattice: Lattice[A]): SuspendM[A] =
        (x, y) match
            // both computations are finished, so the merged one is also finished
            case (Done((eff, v)), Done(eff2, v2)) =>
                Done((eff.merge(eff2), merge(v, v2)))
            case (_: Next[_], _: Next[_]) =>
                Next((env, ctx, state) => merge(x.runNext(env, ctx, state), y.runNext(env, ctx, state)))
            case (_, _) =>
                Next((env, ctx, state) => Fork(lattice, (env, ctx, state), x, y))

    //case (ModularMonadDone((eff, v)), s: ModularMonadSuspend) =>

    /** Suspend the current computation and save its state */
    protected def suspend[X](v: X): SuspendM[X] =
        Next((env, ctx, eff) => Suspended((env, ctx, eff), v))

    protected given suspendAnalysisM[M[_]]: AnalysisM[SuspendM] with
        private type A[X] = SuspendM[X]
        private val mon: Monad[SuspendM] = me
        export mon.*

        /**
         * Reads the context and spawns a new component for <code>lam</code>.
         *
         * @note
         *   assumes that the argument values are already written to the corresponding addresses
         */
        def call(lam: Lam): A[Value] =
            for
                ctx <- getCtx
                env <- getEnv
                cmp = Call(lam, env, ctx)
                _ <- spawn(cmp)
                vlu <- read(ReturnAddr(cmp, lam.idn))
            yield vlu

        /** Looks up the given address in the global store */
        def lookupSto(a: Adr): A[Value] =
            read(a)

        /** Extend the global store with the given value */
        def extendSto(a: Adr, v: Value): A[Unit] =
            write(a, v)

        /** Update the store with the given value */
        def updateSto(a: Adr, v: Value): A[Unit] = extendSto(a, v)

        /** Pointer equality */
        def addrEq: A[MaybeEq[Adr]] = unit(new MaybeEq[Adr] {
            def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
                if a1 == a2 then BoolLattice[B].top else BoolLattice[B].inject(false)
        })

        override def mbottom[X]: A[X] = Next((_, _, e) => Done(e, None))
        override def mjoin[X: Lattice](x: SuspendM[X], y: SuspendM[X]): SuspendM[X] =
            merge(x, y)
        override def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            Next((e, ctx, eff) => blk.withState(f(e), ctx, eff))
        override def getEnv: A[Env] =
            Next((e, _, eff) => Done(eff, Some(e)))
        override def getCtx: A[Ctx] =
            Next((e, ctx, eff) => Done(eff, Some(ctx)))
        override def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            Next((e, ctx, eff) => blk.withState(e, f(ctx), eff))
        override def fail[X](err: Error): SuspendM[X] = {
            println(err); mbottom
        }
