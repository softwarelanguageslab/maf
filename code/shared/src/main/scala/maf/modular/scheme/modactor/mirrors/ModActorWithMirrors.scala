package maf.modular.scheme.modactor.mirrors

import maf.core.Monad.*
import maf.modular.scheme.modactor.GlobalStoreModActor
import maf.core.Lattice
import maf.modular.scheme.modactor.ASchemeMirrorsSemantics
import maf.language.scheme.SchemeExp
import maf.modular.scheme.modactor.*
import maf.lattice.HMap
import maf.modular.scheme.modactor.MirrorValues.Mirror
import maf.core.LatticeTopUndefined
import monocle.macros.GenLens
import maf.language.AScheme.ASchemeValues
import maf.language.AScheme.ASchemeValues.ASchemeValue

trait ModActorWithMirrors extends GlobalStoreModActor, ASchemeMirrorsSemantics:
    /** Encodes whether an actor has a mirror or not */
    enum HasMirror:
        /** We don't know anything, but if asked to lookup we return NoMirror */
        case Bottom

        /** The actor has no mirror at run time */
        case NoMirror

        /** The actor only has a mirror at run time, and does not possible have a path where it does not have a mirror */
        case Mirror(mirrors: Set[MirrorValues.Mirror[ASchemeValues.Actor]])

        /** The actor has a mirror, but at some paths also does not have a mirror */
        case Both(mirror: Set[MirrorValues.Mirror[ASchemeValues.Actor]])

    object HasMirror:
        def join(x: HasMirror, y: HasMirror): HasMirror =
            import HasMirror.*
            (x, y) match
                case (Bottom, y)                => y
                case (x, Bottom)                => x
                case (NoMirror, Mirror(ms))     => Both(ms)
                case (Mirror(ms), NoMirror)     => Both(ms)
                case (Mirror(ms1), Mirror(ms2)) => Mirror(ms1 ++ ms2)
                case (Both(ms1), Both(ms2))     => Both(ms1 ++ ms2)
                case (Mirror(ms1), Both(ms2))   => Both(ms1 ++ ms2)
                case (Both(ms1), Mirror(ms2))   => Both(ms1 ++ ms2)
                case (NoMirror, Both(ms))       => Both(ms)
                case (Both(ms), NoMirror)       => Both(ms)
                case (NoMirror, NoMirror)       => NoMirror

    case class IntraMirrorState(
        /** The state that keeps track of effects and global stores */
        inner: globalStore.IntraState,
        /** A flow insenstive store of mirrors */
        mirrors: Map[ActorRef, HasMirror] = Map().withDefaultValue(HasMirror.Bottom))

    case class InterMirrorState(
        /** The state that keeps track of mailboxes, spawned components, ... */
        inner: globalStore.InterState,
        /** A flow insensitvie store of mirrors */
        mirrors: Map[ActorRef, HasMirror] = Map().withDefaultValue(HasMirror.Bottom))

    type State = IntraMirrorState
    type Inter = InterMirrorState

    implicit override val analysisM: GlobalMetaSemanticsM

    class GlobalMetaSemanticsM extends GlobalStoreAnalysisM, MetaAnalyisM[A] {
        override def currentBehavior: A[Behavior] =
            selfActorCmp flatMap {
                case a @ ActorAnalysisComponent(Actor(beh, _, _), None, _)                => unit(beh)
                case a @ ActorAnalysisComponent(_, Some(BehaviorComponent(beh, _, _)), _) => unit(beh)
                case c =>
                    println(s"+++ meta/receive component is $c which does not satisfy the currentBehavior constraints")
                    mbottom
            }
        override def baseFail(error: Value): A[Unit] = ??? // TODO: implement
        override def isFail(vlu: Value): A[Boolean] = ??? // TODO: implement
        override def lookupMirror(actor: ActorRef): A[Option[Mirror[ActorRef]]] =
            for
                mirror <- get.map(_.mirrors(actor))
                result <- nondets(mirror match
                    case HasMirror.NoMirror | HasMirror.Bottom => Set(unit(None))
                    case HasMirror.Mirror(mirrors)             => mirrors.map(Some.apply[Mirror[ActorRef]] andThen unit)
                    case HasMirror.Both(mirrors)               => mirrors.map(Some.apply[Mirror[ActorRef]] andThen unit) + unit(None)
                )
            yield result

        override def installMirror(forActor: Value, m: Mirror[ActorRef], strong: Boolean = false): A[Unit] =
            for
                st <- get
                actor <- nondets(lattice.getActors(forActor).map(unit))
                mirror = st.mirrors(actor)
                // since we are flow insensitive we can only say that there is both no mirror and a mirror installed
                _ <- put(st.copy(mirrors = st.mirrors + (actor -> (mirror match
                    case _ if strong                                      => HasMirror.Mirror(Set(m))
                    case HasMirror.NoMirror | HasMirror.Bottom if !strong => HasMirror.Both(Set(m))
                    case HasMirror.Mirror(ms)                             => HasMirror.Both(ms + m)
                    case HasMirror.Both(ms)                               => HasMirror.Both(ms + m)
                ))))
            yield ()

    }

    override protected def injectOther(inter: Inter, cmp: Component): A[Unit] =
        analysisM.get.map(intra => intra.copy(mirrors = inter.mirrors)) >>= analysisM.put

    override def syncInter(intra: Intra, inter: Inter): Inter =
        val newMirrors = intra.mirrors.foldLeft(inter.mirrors) { case (mirrors, (cmp, mirror)) =>
            mirrors + (cmp -> HasMirror.join(mirrors(cmp), mirror))
        }
        val innerSync = globalStore.sync(intra.inner, inter.inner)
        InterMirrorState(innerSync, newMirrors)

    override val intraLens = GenLens[IntraMirrorState](_.inner)
    override val interLens = GenLens[InterMirrorState](_.inner)

    override protected def initialInterState(inter: globalStore.InterState): Inter =
        InterMirrorState(inter)
    override protected def initialIntra(cmp: Component, intra: globalStore.IntraState): Intra =
        IntraMirrorState(intra)

class SimpleModActorWithMirrors(prog: SchemeExp) extends SchemeModActorSemantics(prog), ModActorWithMirrors:
    def messageCtx(mCtx: MessageContext)(ctx: Ctx): Ctx = MsgCtxContext(mCtx)

    override def newContext(fex: Exp, lam: Lam, ags: List[Val], ctx: Ctx): Ctx = ctx
    implicit override val analysisM: GlobalMetaSemanticsM = new GlobalMetaSemanticsM()
    def reifyMessage(m: Msg, exs: List[SchemeExp]): ASchemeValues.Message[Value] = m.copy(exs = exs)
    def abstractMessage(m: Msg): ASchemeValues.Message[Value] = m
