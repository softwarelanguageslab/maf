package maf.modular.scheme.modactor

import maf.language.scheme.SchemeExp
import maf.modular.ModAnalysis
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.core.worklist.FIFOWorkList
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.modular.scheme.modconc.StandardSchemeModConcAllocator
import maf.language.AScheme.ASchemeValues.Behavior
import maf.modular.scheme.modf.StandardSchemeModFAllocator
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.worklist.RandomWorklistAlgorithm

class SimpleSchemeModActorAnalysis(program: SchemeExp)
    extends ModAnalysis[SchemeExp](program)
    with SchemeConstantPropagationDomain
    with StandardSchemeModActorAllocator
    with ModActorNoSensitivity
    with SimpleMessageMailbox
    with PowersetMailboxAnalysis
    with StandardSchemeModActorComponents
    with FIFOWorklistAlgorithm[SchemeExp]
    with SchemeModActorSemantics:

    override def intraAnalysis(component: SchemeModActorComponent[ComponentContext]): ModActorIntra = new ModActorIntra(component)
    override def innerModF(intra: ModActorIntra, beh: Behavior): InnerModF =
        new InnerModF(intra, beh) with SchemeModFNoSensitivity with RandomWorklistAlgorithm[SchemeExp] {}
