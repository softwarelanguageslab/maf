package maf.modular

import maf.core.Expression

/**
  * By mixing in this trait, the store can be (unsoundly) destructively updated (i.e., without joining and triggering
  * dependencies)
  */
trait DestructiveStore[Expr <: Expression]
    extends ModAnalysis[Expr]
    with GlobalStore[Expr]
    with AbstractDomain[Expr] {

  override def intraAnalysis(cmp: Component): DestructiveStoreIntra
  trait DestructiveStoreIntra extends GlobalStoreIntra {
    def forceWrite(addr: Addr, value: Value): Unit = store = store.updated(addr, value)
  }
}
