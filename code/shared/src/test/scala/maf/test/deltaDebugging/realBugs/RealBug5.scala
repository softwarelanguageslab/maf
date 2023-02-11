package maf.test.deltaDebugging.realBugs

import maf.language.scheme.SchemeExp
import maf.modular.GlobalStore

trait RealBug5 extends GlobalStore[SchemeExp]:
  trait BuggedStore extends GlobalStoreIntra:
    override def readAddr(addr: Addr): Value =
      store.get(addr) match {
        case None =>
          store += (addr -> lattice.bottom)
          lattice.bottom
        case Some(v) => v
      }