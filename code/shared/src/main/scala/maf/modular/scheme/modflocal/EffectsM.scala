package maf.modular.scheme.modflocal

import maf.core.*
import maf.core.Monad.*
import maf.modular.Dependency
import maf.util.Lens

trait EffectLens[S] extends Lens[S]:
    def putWrites(s: S, w: Set[Address]): S
    def getWrites(s: S): Set[Address]
    def writes = (putWrites, getWrites)

    def putReads(s: S, w: Set[Dependency]): S
    def getReads(s: S): Set[Dependency]
    def reads = (putReads, getReads)

trait EffectsM[M[_], Cmp, S: EffectLens] extends StateOps[S, M]:
    private given Monad[M] = this
    private val lens: EffectLens[S] = summon[EffectLens[S]]

    /** The set of write effects */
    def ws: M[Set[Address]]

    /** The set of read effects */
    def rs: M[Set[Dependency]]

    /** The set of call effects */
    def cs: M[Set[Cmp]]

    /** Register a write effect */
    def trigger(w: Address): M[Unit] =
        get.map(lens.modify(lens.writes)(_ + w)) >>= put

    /** Register a read effect */
    def register(d: Dependency): M[Unit] =
        get.map(lens.modify(lens.reads)(_ + d)) >>= put
