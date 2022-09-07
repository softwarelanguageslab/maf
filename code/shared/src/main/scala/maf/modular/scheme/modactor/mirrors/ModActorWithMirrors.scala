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

trait ModActorWithMirrors extends GlobalStoreModActor, ASchemeMirrorsSemantics:
    /** Encodes whether an actor has a mirror or not */
    enum HasMirror:
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
        mirrors: Map[ActorRef, HasMirror] = Map().withDefaultValue(HasMirror.NoMirror))

    case class InterMirrorState(
        /** The state that keeps track of mailboxes, spawned components, ... */
        inner: globalStore.InterState,
        /** A flow insensitvie store of mirrors */
        mirrors: Map[ActorRef, HasMirror] = Map().withDefaultValue(HasMirror.NoMirror))

    type State = IntraMirrorState
    type Inter = InterMirrorState

    implicit override val analysisM: GlobalMetaSemanticsM

    class GlobalMetaSemanticsM extends GlobalStoreAnalysisM, MetaAnalyisM[A] {
        override def lookupMirror(actor: ActorRef): A[Option[Mirror[ActorRef]]] =
            for
                mirror <- get.map(_.mirrors(actor))
                result <- nondets(mirror match
                    case HasMirror.NoMirror        => Set(unit(None))
                    case HasMirror.Mirror(mirrors) => mirrors.map(Some.apply[Mirror[ActorRef]] andThen unit)
                    case HasMirror.Both(mirrors)   => mirrors.map(Some.apply[Mirror[ActorRef]] andThen unit) + unit(None)
                )
            yield result

        override def installMirror(forActor: Value, m: Mirror[ActorRef]): A[Unit] =
            for
                st <- get
                actor <- nondets(lattice.getActors(forActor).map(unit))
                mirror = st.mirrors(actor)
                // since we are flow insensitive we can only say that there is both no mirror and a mirror installed
                _ <- put(st.copy(mirrors = st.mirrors + (actor -> (mirror match
                    case HasMirror.NoMirror   => HasMirror.Both(Set(m))
                    case HasMirror.Mirror(ms) => HasMirror.Both(ms + m)
                    case HasMirror.Both(ms)   => HasMirror.Both(ms + m)
                ))))
            yield ()

    }

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
    implicit override val analysisM: GlobalMetaSemanticsM = new GlobalMetaSemanticsM()
