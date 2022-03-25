package maf.cli.experiments.clients

import maf.core.Identity
import maf.language.scheme.*
import maf.language.ContractScheme.*

trait ScvClientAnalysis extends ClientAnalysisRunner:
    def parseProgram(txt: String): SchemeExp =
        SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)
