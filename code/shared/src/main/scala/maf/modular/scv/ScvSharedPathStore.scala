package maf.modular.scv

/** Shares the entire path store (after restoring renames of symbolic variables) across function call components. */
trait ScvSharedPathStore extends maf.modular.scv.BaseScvBigStepSemantics:
    override def intraAnalysis(component: Component): SharedPathStoreIntra

    trait SharedPathStoreIntra extends BaseIntraScvSemantics
