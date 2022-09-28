package maf.modular.scv.actors

import maf.modular.scheme.modactor.*
import maf.core.*
import maf.core.Monad
import maf.core.Monad.*
import maf.language.AContractScheme.*
import maf.language.AContractScheme.AContractSchemeValues.*
import maf.language.scheme.*

/** Semantics for evaluating contract scheme for actors */
trait AContractSchemeSemantics extends ASchemeSemantics, AContractSchemeDomain:
    import analysisM.*

    override def eval(e: SchemeExp): A[Value] = e match
        case AContractSchemeMessage(tag, argumentContracts, ensureContract, _) =>
            for
                argumentContractsEvaluated <- argumentContracts.mapM(eval)
                // TODO[easy]: should track some error here for the values that are not ensures/c
                ensureContractEvaluated <- eval(ensureContract) >>= (lattice.getEnsuresC andThen (_.map(unit)) andThen nondets)
            yield lattice.messageC(MessageC(tag, argumentContractsEvaluated, ensureContractEvaluated))

        case SchemeFuncall(SchemeVar(Identifier("ensures/c", _)), messages, _) =>
            for
                // TODO[easy]: should track some error here for the values that are not ensures/c
                evaluatedMessages <- messages.mapM(eval >=> (lattice.getMessageC andThen (_.map(unit)) andThen nondets))
            yield lattice.ensuresC(EnsuresC(evaluatedMessages))

        case _ => super.eval(e)
