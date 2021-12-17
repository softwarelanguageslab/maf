package maf.modular.scheme.modflocal

import maf.core._
import maf.modular._
import maf.modular.scheme._
import maf.language.scheme._
import scala.annotation.tailrec

trait SchemeModFLocalGlobalStore extends GlobalStore[SchemeExp]:
    this: SchemeDomain with SchemeModFLocalSensitivity with SchemeModFLocal =>

    var refs: Map[Adr, Set[Adr]] = Map.empty
    var lcls: Map[Adr, Set[Adr]] = Map.empty

    // NOTE: not suitable for parallelisation
    // TODO: writeAddr only happens after the analysis?
    override def writeAddr(adr: Adr, vlu: Val): Boolean =
        val hasChanged = super.writeAddr(adr, vlu)
        if hasChanged then
            val addrs = lattice.refs(vlu)
            // update refs
            addrs.foreach { oth =>
              refs += oth -> (refs.getOrElse(oth, Set.empty) + adr)
            }
            // update lcls
            val cur = addrs.flatMap { oth =>
              policy(oth) match
                  case AddrPolicy.Local   => Set(oth)
                  case AddrPolicy.Widened => lcls.getOrElse(oth, Set.empty)
            }
            propagate(adr, cur)
        hasChanged

    private def propagate(adr: Adr, upd: Set[Adr]): Unit =
        val prv = lcls.getOrElse(adr, Set.empty)
        val pro = upd -- prv
        if pro.nonEmpty then
            lcls += adr -> (prv ++ pro)
            trigger(RefsDependency(adr))
            refs.getOrElse(adr, Set.empty).foreach(propagate(_, pro))

    case class RefsDependency(addr: Adr) extends Dependency

    override def intraAnalysis(cmp: Cmp): SchemeModFLocalGlobalStoreIntra
    trait SchemeModFLocalGlobalStoreIntra extends GlobalStoreIntra:
        def readRefs(adr: Adr): Set[Adr] =
            register(RefsDependency(adr))
            lcls.getOrElse(adr, Set.empty)
